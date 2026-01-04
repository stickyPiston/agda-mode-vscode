module AgdaMode.Extension.Highlighting where

open import Prelude.List
open import Prelude.Nat
open import Prelude.String
open import Prelude.Maybe using (Maybe ; just ; nothing)
open import Prelude.JSON.Decode
open import Prelude.JSON
open import Iepje.Internal.Utils using (case_of_ ; if_then_else_)
open import Prelude.Function
open import Agda.Builtin.Bool
open import Vscode.SemanticTokensProvider
open import Prelude.Sigma
open import Prelude.Vec hiding (slice ; map ; for)
open import Prelude.Function
open import TEA.System

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
        position : ℕ

definition-site-decoder : Decoder DefinitionSite
definition-site-decoder = ⦇ mk-DefinitionSite (required "filepath" string) (required "position" nat) ⦈

record Token : Set where
    constructor mk-Token
    field
        atoms : Σ[ n ∈ ℕ ] Vec Aspect (suc n)
        definition-site : Maybe DefinitionSite
        note : String
        start end : ℕ
        token-based : Bool

token-decoder : Decoder Token
token-decoder = ⦇ mk-Token
    (required "atoms" (non-empty-vec aspect-decoder))
    (optional-null "definitionSite" definition-site-decoder)
    (required "note" string)
    (required "range" (list nat |> index 0)) (required "range" (list nat |> index 1))
    (required "tokenBased" string <&> primStringEquality "TokenBased") ⦈

highlighting-info-decoder : Decoder (List Token)
highlighting-info-decoder =  list token-decoder |> required "payload" |> required "info"

-- Conversion function from highlighting atoms to legend token types + modifiers
aspect→legend : ∀ {n} → Vec Aspect (suc n) → String × List String
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

divide-ranges : vscode-api → TextDocument.t → Range.t → List Range.t
divide-ranges vscode doc r = go (line (start r) - line (end r))
    where
        open Position
        open Range

        single-line-range : ℕ → Range.t
        single-line-range n =
            let full-line-range = TextLine.range (TextDocument.line-at doc (line (start r) + n))
             in Range.new vscode
                    (start (if n == zero then r else full-line-range))
                    (end (if n == line (end r) - line (start r) then r else full-line-range))

        go : ℕ → List Range.t
        go zero = single-line-range zero ∷ []
        go (suc n) = single-line-range n ∷ go n

make-highlighting-tokens : vscode-api → TextDocument.t → List Token → List SemanticToken.t
make-highlighting-tokens vscode doc = concat-map λ token →
    let original-range = Range.new vscode (TextDocument.position-at doc (token .start - 1)) (TextDocument.position-at doc (token .end - 1))
        single-line-ranges = divide-ranges vscode doc original-range
        token-type , mods = aspect→legend (token .atoms .Σ.proj₂)
     in for single-line-ranges λ r → record { range = r ; token-type = token-type ; modifiers = mods }
    where open Token