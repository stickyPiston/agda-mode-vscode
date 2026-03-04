module AgdaMode.Extension.Keymap where

open import Data.List renaming (∥_∥ to ∥_∥ˡ ; slice to sliceˡ)
import Data.List.Zipper as List
open import Data.JSON
open import Data.JSON.Decode
open import Data.String renaming (_++_ to _++ˢ_ ; ∥_∥ to ∥_∥ˢ)
open import Data.IO
open import Data.Maybe
open import Data.Maybe.Effectful
open import Data.Map
open import Data.Product
open import Data.Nat renaming (_==_ to _≡ⁿ_)
open import Data.Bool
open import Agda.Builtin.Unit
open import Function

open import Effect.Monad

open import Node.FileSystem

private variable A : Set

open Monad ⦃ ... ⦄
open MonadPlus ⦃ ... ⦄ using (_<|>_ ; ⊘)

module Trie where
  record t : Set where
    inductive
    constructor trie
    field
      values : List String
      subtrees : List (String × t)
  open t public

  empty : t
  empty = record { values = [] ; subtrees = [] }

open Trie using (trie ; values ; subtrees) public

{-# TERMINATING #-}
trie-decoder : Decoder Trie.t
trie-decoder =
  let values-decoder = from-Maybe [] <$> optional ">>" (list string)
      subtrees-decoder = λ where
        (j-object o) → just $ concat-for (StringMap.entries o) λ where
          (">>" , _) → []
          (k , v) → case trie-decoder v of λ where
            (just t) → [ k , t ]
            nothing → []
        _ → nothing
   in ⦇ trie values-decoder subtrees-decoder ⦈

load-keymap : String → IO (Maybe Trie.t)
load-keymap = fmap (parse-json >=> trie-decoder) ∘ load-file

-- Θ(min(|xs|, depth(t)))
match : List String → Trie.t → Maybe Trie.t
match [] t = just t
match (x ∷ xs) t = find (λ (k , _) → k == x) (t .subtrees) >>= match xs ∘ proj₂

-- Suggest which characters can be used to continue traverse the trie
next-characters : Trie.t → List String
next-characters = sort ∘ map proj₁ ∘ subtrees

module Zipper where
  record Ancestor : Set where
    inductive
    field
      children : List (String × Trie.t)
      values : List String
      parent : Maybe (String × Ancestor)
  open Ancestor

  record t : Set where
    field
      ancestor : Maybe (String × Ancestor)
      values : List.Zipper.t String
      subtrees : List (String × Trie.t)
  open t public

  from-trie : Trie.t → t
  from-trie t = record
    { ancestor = nothing
    ; values = List.Zipper.from-list (t .values)
    ; subtrees = t .subtrees
    }

  go-up : t → Maybe t
  go-up record { ancestor = ancestor ; values = values ; subtrees = subtrees } = do
    edge , record { parent = parent ; children = children ; values = values } ← ancestor
    pure record { ancestor = parent ; values = List.Zipper.from-list values ; subtrees = children }

  go-down : String → t → Maybe t
  go-down c record { ancestor = ancestor ; values = vs ; subtrees = ts } = do
    _ , subtree ← find (λ (e , _) → e == c) ts
    let ancestor = record
          { children = ts
          ; values = List.Zipper.to-list vs
          ; parent = ancestor
          }
    pure record
      { ancestor = just (c , ancestor)
      ; values = List.Zipper.from-list (subtree .values)
      ; subtrees = subtree .subtrees
      }

  go-left go-right : t → t
  go-left z@record { values = vs } = record z { values = List.Zipper.go-left vs }
  go-right z@record { values = vs } = record z { values = List.Zipper.go-right vs }

  edges : t → List String
  edges record { subtrees = subtrees } = map proj₁ subtrees

  current-prefix : t → String
  current-prefix record { ancestor = ancestor } = go ancestor
    where
      go : Maybe (String × Ancestor) → String
      go (just (e , ancestor)) = go (ancestor .parent) ++ˢ e
      go nothing = ""

module InputMode where
  open import Vscode.Common
  open import Vscode.Command
  open import Vscode.TextEditor
  open import Vscode.Window
  open Zipper hiding (t)

  record t : Set where field
    start-pos : Position.t
    zipper : Zipper.t
  open t public

  Model : Set
  Model = Maybe t

  data Msg : Set where
    backspace left right : Msg
    character : String → Msg

  navigate : (Zipper.t → Maybe Zipper.t) → t → Model
  navigate f model = f (model .zipper) <&> λ z → record model { zipper = z }

  -- Vscode does not send backspace, left and right when we are not in input mode,
  -- i.e. when the model is nothing
  msg-for : Model → Set
  msg-for = maybe String (const Msg)

  -- TODO: I'd love to get rid of the IO in this function and have a pure
  --       update function that could even be proved correct
  -- TODO: Check that vim mode is in insert mode if it is enabled
  update : (Trie.t × TextEditor.t) → (m : Model) → msg-for m → IO Model
  update (_ , e) (just model) backspace = case navigate Zipper.go-up model of λ where
    (just m) → just m <$ do
      here ← TextEditor.cursor-pos e
      TextEditor.edit [ Edit.delete (Range.new (Position.left 1 here) here) ] e
    nothing → pure nothing
  update _ (just model) left = pure $ navigate (just ∘ Zipper.go-left) model
  update _ (just model) right = pure $ navigate (just ∘ Zipper.go-right) model
  update _ (just model) (character c) =
    navigate (Zipper.go-down c) model <$
      execute-command "default:type" (j-object ("text" ↦ j-string c))

  update (keymap , e) nothing "\\" = do
    execute-command "default:type" (j-object ("text" ↦ j-string "\\"))

    -- Create a fresh imb and insert it into the model
    TextEditor.cursor-pos e <&> λ start-pos → just record
      { start-pos = Position.left 1 start-pos
      ; zipper = Zipper.from-trie keymap
      }

  update _ nothing c = nothing <$ execute-command "default:type" (j-object ("text" ↦ j-string c))

  page-size : ℕ
  page-size = 10

  paginate-values : List.Zipper.t A → List A
  paginate-values record { right = r ; rev-left = rev-left } =
    let page , offset = ∥ rev-left ∥ˡ quot-rem page-size
     in reverse (take offset rev-left) ++ take (page-size - offset) r

  imb-text : t → String
  imb-text imb =
    let next = join $ Zipper.edges (imb .zipper)
        candidates =
          imb .zipper .values
          |> List.Zipper.map-focus (λ c → "[" ++ˢ c ++ˢ "]")
          |> paginate-values
          |> intercalate ", "
          |> " $(chevron-right) " ++ˢ_
     in "\\" ++ˢ Zipper.current-prefix (imb .zipper) ++ˢ "[" ++ˢ next ++ˢ "]" ++ˢ candidates

  view : TextEditor.t → DecorationType.t → StatusBarItem.t → Model → IO Model
  view e d stb nothing  = do
    TextEditor.remove-decoration d e
    execute-command₂ "setContext" "agda-mode.inInputMode" false
    StatusBarItem.hide stb
    pure nothing

  view e d stb (just m) = do
    -- Underline what is typed so far
    range ← Range.new (m .start-pos) <$> TextEditor.cursor-pos e
    TextEditor.set-decoration d range e

    -- Enable `inInputMode` context, so that vscode sends backspace and arrow key events
    execute-command₂ "setContext" "agda-mode.inInputMode" true

    -- Update the status bar with the new possibilities
    StatusBarItem.set-text stb (imb-text m)
    StatusBarItem.show stb

    -- Set the correct replacement
    let z = m .zipper
    case List.Zipper.focus (z .values) of λ where
      (just c) → do
        end-pos ← TextEditor.cursor-pos e
        let r = Range.new (m .start-pos) end-pos
        TextEditor.edit [ Edit.replace r c ] e
        pure tt
      nothing → pure tt

    -- If there is just one possibility left, exit input mode already
    if ∥ List.Zipper.to-list (z .values) ∥ˡ ≤ 1 ∧ null? (z .subtrees)
      then view e d stb nothing
      else pure (just m)
