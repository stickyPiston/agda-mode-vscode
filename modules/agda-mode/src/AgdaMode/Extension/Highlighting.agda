module AgdaMode.Extension.Highlighting where

open import Prelude.List
open import Prelude.Nat
open import Prelude.String
open import Prelude.Maybe using (Maybe ; just ; nothing)
open import Prelude.JSON.Decode
open import Prelude.JSON
open import Iepje.Internal.Utils using (case_of_)
open import Prelude.Function
open import Agda.Builtin.Bool
open import Vscode.SemanticTokensProvider renaming (string to string'')
open import Prelude.Sigma

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
        atoms : List Aspect
        definition-site : Maybe DefinitionSite
        note : String
        start end : ℕ
        token-based : Bool

token-decoder : Decoder Token
token-decoder = ⦇ mk-Token
    (required "atoms" (list aspect-decoder))
    (optional-null "definitionSite" definition-site-decoder)
    (required "note" string)
    (required "range" (list nat |> index 0)) (required "range" (list nat |> index 1))
    (required "tokenBased" string <&> primStringEquality "TokenBased") ⦈

-- Conversion function from highlighting atoms to legend token types + modifiers
aspect→legend : Aspect → DefaultTokenType × DefaultModifier
aspect→legend = {!   !}

agda-token→semantic-token : Token → SemanticToken
agda-token→semantic-token = {!   !}
