module AgdaMode.Extension.Highlighting where

open import Effect.Applicative
open import Effect.Monad

open import Data.JSON.Decode as Decode
open import Data.JSON
import Data.IO as IO
open IO using (IO)

open import Data.List
open import Data.Product
open import Agda.Builtin.Nat renaming (_==_ to _==ⁿ_)
open import Data.String
open import Data.Maybe using (Maybe ; just ; nothing)
open import Agda.Builtin.Bool
open import Function

open import Vscode.Common
open import Vscode.SemanticTokensProvider

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

definition-site-decoder : Decoder DefinitionSite
definition-site-decoder = ⦇ mk-DefinitionSite (required "filepath" string) (required "position" nat) ⦈
  where open Applicative Decode.applicative

record Token : Set where
    constructor mk-Token
    field
        atoms : List Aspect
        definition-site : Maybe DefinitionSite
        note : String
        start end : Nat
        token-based : Bool

token-decoder : Decoder Token
token-decoder = ⦇ mk-Token
    (required "atoms" (list aspect-decoder))
    (optional-null "definitionSite" definition-site-decoder)
    (required "note" string)
    (required "range" (list nat |> index 0)) (required "range" (list nat |> index 1))
    (required "tokenBased" string <&> ("TokenBased" ==_)) ⦈
  where open Applicative applicative

highlighting-info-decoder : Decoder (List Token)
highlighting-info-decoder =
  required "kind" string >>= λ where
    "HighlightingInfo" → list token-decoder |> required "payload" |> required "info"
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

divide-ranges : TextDocument.t → Range.t → IO (List Range.t)
divide-ranges doc r = go (line (start r) - line (end r))
    where
        open Position
        open Range
        open Monad IO.Effectful.monad

        single-line-range : Nat → IO Range.t
        single-line-range n =
            let full-line-range = TextLine.range (TextDocument.line-at doc (line (start r) + n))
             in Range.new
                    (start (if n ==ⁿ zero then r else full-line-range))
                    (end (if n ==ⁿ line (end r) - line (start r) then r else full-line-range))

        go : Nat → IO (List Range.t)
        go zero = (_∷ []) <$> single-line-range zero
        go (suc n) = ⦇ single-line-range n ∷ go n ⦈

make-highlighting-tokens : TextDocument.t → List Token → IO (List SemanticToken.t)
make-highlighting-tokens doc = (concat <$>_) ∘ mapA to-semantic-token
    where
      open Token
      open Monad IO.Effectful.monad
      open TraversableA IO.Effectful.applicative

      to-semantic-token : Token → IO (List SemanticToken.t)
      to-semantic-token token = do
        original-range ← Range.new (TextDocument.position-at doc (token .start - 1)) (TextDocument.position-at doc (token .end - 1))
        single-line-ranges ← divide-ranges doc original-range
        let token-type , mods = aspect→legend (token .atoms)
        pure $ map (λ r → record { range = r ; token-type = token-type ; modifiers = mods }) single-line-ranges
