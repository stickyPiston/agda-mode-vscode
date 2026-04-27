module AgdaMode.Extension.Highlighting where

open import Effect.Applicative
open import Effect.Monad

open import Data.JSON.Decode as Decode
open import Data.JSON
import Data.IO as IO
open IO using (IO)

open import Data.Bool
open import Data.List hiding (_++_)
open import Data.Product
open import Data.Nat renaming (_==_ to _==ⁿ_) hiding (show)
import Data.Nat as Nat
open import Data.String hiding (show ; _==_)
import Data.String as String
open import Data.Maybe using (Maybe ; just ; nothing)
open import Agda.Builtin.Bool
open import Agda.Builtin.Unit
open import Function

open import Vscode.Common
open import Vscode.SemanticTokensProvider
open import Vscode.TextEditor

private postulate trace : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} → A → B → B
{-# COMPILE JS trace = _ => _ => _ => _ => thing => val => { console.log(thing) ; return val } #-}

data Aspect : Set where
    background markup symbol inductive-constructor string' postulate' function pragma : Aspect
    hole comment keyword number primitive-type error dotted-pattern unsolved-meta unsolved-constraint : Aspect
    termination-problem positivity-problem dead-code coverage-problem incomplete-pattern type-checks : Aspect
    catchall-clause confluence-problem bound generalizable coinductive-constructor datatype field' : Aspect
    module' primitive' record' argument operator : Aspect

aspect-decoder : Decoder Aspect
aspect-decoder (j-string s) = case s of λ where
    "background" → just background ; "markup" → just markup ; "symbol" → just symbol
    "inductiveconstructor" → just inductive-constructor ; "string" → just string'
    "postulate" → just postulate' ; "function" → just function ; "pragma" → just pragma
    "hole" → just hole ; "comment" → just comment ; "keyword" → just keyword ; "number" → just number
    "primitivetype" → just primitive-type ; "error" → just error ; "dottedpattern" → just dotted-pattern
    "unsolvedmeta" → just unsolved-meta ; "unsolvedconstraint" → just unsolved-constraint
    "terminationproblem" → just termination-problem ; "positivityproblem" → just positivity-problem
    "deadcode" → just dead-code ; "coverageproblem" → just coverage-problem ; "incompletepattern" → just incomplete-pattern
    "typechecks" → just type-checks ; "catchallclause" → just catchall-clause
    "confluenceproblem" → just confluence-problem ; "bound" → just bound ; "generalizable" → just generalizable
    "coinductiveconstructor" → just coinductive-constructor ; "datatype" → just datatype ; "field" → just field'
    "module" → just module' ; "primitive" → just primitive' ; "record" → just record'
    "argument" → just argument ; "operator" → just operator
    _ → nothing
aspect-decoder _ = nothing

record DefinitionSite : Set where
  constructor mk-DefinitionSite
  field
    filepath : String
    position : Nat
open DefinitionSite public

definition-site-decoder : Decoder DefinitionSite
definition-site-decoder = ⦇ mk-DefinitionSite (required "filepath" string) (required "position" nat) ⦈
  where open Applicative Decode.applicative

module Token where
  record t : Set where
    constructor mk-Token
    field
      atoms : List Aspect
      definition-site : Maybe DefinitionSite
      note : String
      start end : Nat
      token-based : Bool
  open t public
  
  show : t → String
  show t = "mk-Token { start = " ++ Nat.show (t .start) ++ " ; end = " ++ Nat.show (t .end) ++ "}"
open Token using (atoms ; definition-site ; note ; start ; end ; token-based ; mk-Token) public

token-decoder : Decoder Token.t
token-decoder = ⦇ mk-Token
    (required "atoms" (list aspect-decoder))
    (optional-null "definitionSite" definition-site-decoder)
    (required "note" string)
    (required "range" (list nat |> index 0 |> fmap (_- 1))) (required "range" (list nat |> index 1 |> fmap (_- 1)))
    (required "tokenBased" string <&> ("TokenBased" String.==_)) ⦈
  where open Applicative applicative

highlighting-info-decoder : Decoder (List Token.t × Bool)
highlighting-info-decoder =
  required "kind" string >>= λ where
    "HighlightingInfo" → (_,_ <$> required "payload" (list token-decoder) <*> required "remove" bool) |> required "info"
    _ → ⊘
  where open MonadPlus monad-plus

-- Conversion function from highlighting atoms to legend token types + modifiers
aspect→legend : List Aspect → String × List String
aspect→legend (symbol ∷ _) = "operator" , []
aspect→legend (inductive-constructor ∷ _) = "enumMember" , []
aspect→legend (string' ∷ _) = "string" , []
aspect→legend (postulate' ∷ _) = "function" , []
aspect→legend (function ∷ _) = "function" , []
aspect→legend (comment ∷ _) = "comment" , []
aspect→legend (keyword ∷ _) = "keyword" , []
aspect→legend (number ∷ _) = "number" , []
aspect→legend (primitive-type ∷ _) = "type" , [ "defaultLibrary" ]
aspect→legend (dead-code ∷ _) = "comment" , []
aspect→legend (catchall-clause ∷ _) = "operator" , []
aspect→legend (bound ∷ _) = "parameter" , []
aspect→legend (coinductive-constructor ∷ _) = "enumMember" , []
aspect→legend (datatype ∷ _) = "type" , []
aspect→legend (field' ∷ _) = "property" , []
aspect→legend (module' ∷ _) = "namespace" , []
aspect→legend (primitive' ∷ _) = "string" , [ "defaultLibrary" ]
aspect→legend (record' ∷ _) = "struct" , []
aspect→legend (operator ∷ _) = "operator" , []
aspect→legend _ = "variable" , []

legend : Legend.t
legend = record { TokenType = DefaultTokenType ; Modifier = DefaultModifier }

divide-ranges : TextDocument.t → Range.t → List Range.t
divide-ranges doc r = go (line (Range.start r) - line (Range.end r))
  where
    open Position
    open Range

    single-line-range : Nat → Range.t
    single-line-range n =
        let full-line-range = TextLine.range (TextDocument.line-at doc (line (Range.start r) + n))
          in Range.new
              (Range.start (if n ==ⁿ zero then r else full-line-range))
              (Range.end (if n ==ⁿ line (Range.end r) - line (Range.start r) then r else full-line-range))

    go : Nat → List Range.t
    go zero = [ single-line-range zero ]
    go (suc n) = single-line-range n ∷ go n

open Monad {{ ... }}

module HighlightingDecoration where
  data t : Set where
    unsolved-metas-decoration : t
    termination-decoration : t
    coverage-decoration : t
    positivity-decoration : t
    missing-def-decoration : t
    fatal-warning-decoration : t
    dead-code-decoration : t
    not-definitional-decoration : t
    confluence-decoration : t

  enum : List t
  enum = unsolved-metas-decoration ∷ termination-decoration ∷ coverage-decoration
       ∷ positivity-decoration ∷ missing-def-decoration ∷ fatal-warning-decoration
       ∷ dead-code-decoration ∷ not-definitional-decoration ∷ confluence-decoration
       ∷ []

  from-Aspect : Aspect → List t
  from-Aspect unsolved-meta = [ unsolved-metas-decoration ]
  from-Aspect unsolved-constraint = [ unsolved-metas-decoration ]
  from-Aspect termination-problem = [ termination-decoration ]
  from-Aspect coverage-problem = [ coverage-decoration ]
  from-Aspect positivity-problem = [ positivity-decoration ]
  from-Aspect dead-code = [ dead-code-decoration ]
  from-Aspect catchall-clause = [ not-definitional-decoration ]
  from-Aspect confluence-problem = [ confluence-decoration ]
  from-Aspect _ = []

  _==_ : t → t → Bool
  unsolved-metas-decoration == unsolved-metas-decoration = true
  termination-decoration == termination-decoration = true
  coverage-decoration == coverage-decoration = true
  positivity-decoration == positivity-decoration = true
  missing-def-decoration == missing-def-decoration = true
  fatal-warning-decoration == fatal-warning-decoration = true
  dead-code-decoration == dead-code-decoration = true
  not-definitional-decoration == not-definitional-decoration = true
  confluence-decoration == confluence-decoration = true
  _ == _ = false

  private
    bg-decoration : String → IO DecorationType.t
    bg-decoration colour = DecorationType.Options.new
      <&> DecorationType.Options.set-background-colour colour
      >>= DecorationType.create

  init : t → IO DecorationType.t
  init unsolved-metas-decoration = bg-decoration "#ffff00"
  init termination-decoration = bg-decoration "#ffa07a"
  init coverage-decoration = bg-decoration "#f5deb3"
  init positivity-decoration = bg-decoration "#cd853f"
  init missing-def-decoration = bg-decoration "#ffa500"
  init fatal-warning-decoration = bg-decoration "#f08080"
  init dead-code-decoration = bg-decoration "#a9a9a9"
  init not-definitional-decoration = bg-decoration "#f5f5f5"
  init confluence-decoration = bg-decoration "#ffc0cb"
open HighlightingDecoration using
  ( unsolved-metas-decoration ; termination-decoration ; coverage-decoration
  ; positivity-decoration ; missing-def-decoration ; fatal-warning-decoration
  ; dead-code-decoration ; not-definitional-decoration ; confluence-decoration
  ) public

module HighlightingDecorationMap where
  t : Set
  t = HighlightingDecoration.t → DecorationType.t

  create : IO t
  create = do
    let open HighlightingDecoration
    umd ← init unsolved-metas-decoration ; td ← init termination-decoration
    cd ← init coverage-decoration ; pd ← init positivity-decoration
    mdd ← init missing-def-decoration ; fwd ← init fatal-warning-decoration
    dcd ← init dead-code-decoration ; ndd ← init not-definitional-decoration
    cfd ← init confluence-decoration
    pure λ where
      unsolved-metas-decoration → umd ; termination-decoration → td
      coverage-decoration → cd ; positivity-decoration → pd
      missing-def-decoration → mdd ; fatal-warning-decoration → fwd
      dead-code-decoration → dcd ; not-definitional-decoration → ndd
      confluence-decoration → cfd

open TraversableM {{ ... }}

token-position : TextDocument.t → Token.t → List Range.t
token-position doc token =
  let original-range = Range.new (TextDocument.position-at doc (token .start)) (TextDocument.position-at doc (token .end)) in
  divide-ranges doc original-range

apply-decorations : TextEditor.t → TextDocument.t → HighlightingDecorationMap.t → List Token.t → IO ⊤
apply-decorations ed doc hd-map tokens = do
  let all-decorations = concat-map (λ t → concat-map (λ a → map (t ,_) (HighlightingDecoration.from-Aspect a)) (atoms t)) tokens
  HighlightingDecoration.enum |> mapM λ dec → do
    let specific-tokens = all-decorations |> map-Maybe λ (t , d) → if d HighlightingDecoration.== dec then just t else nothing
    let positions = concat-map (token-position doc) specific-tokens
    TextEditor.set-decoration (hd-map dec) positions ed
  pure tt

make-highlighting-tokens : TextDocument.t → List Token.t → List SemanticToken.t
make-highlighting-tokens doc = concat ∘ map to-semantic-token
  where
    to-semantic-token : Token.t → (List SemanticToken.t)
    to-semantic-token token =
      let single-line-ranges = token-position doc token
          token-type , mods = aspect→legend (token .atoms)
       in map (λ r → record { range = r ; token-type = token-type ; modifiers = mods }) single-line-ranges
