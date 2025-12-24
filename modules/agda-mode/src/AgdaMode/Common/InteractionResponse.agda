module AgdaMode.Common.InteractionResponse where

open import Prelude.Maybe
open import Prelude.Nat hiding (_==_)
open import Prelude.Sigma
open import AgdaMode.Common.Communication
open import AgdaMode.Common.JSON

open import Agda.Builtin.List
open import Agda.Builtin.String
open import Agda.Builtin.Bool

open import Iepje.Prelude using (case_of_ ; if_then_else_)

private
    postulate trace : ∀ {A B : Set} → A → B → B
    {-# COMPILE JS trace = _ => _ => thing => ret => { console.log(thing); return ret } #-}

postulate _starts-with_ : String → String → Bool
{-# COMPILE JS _starts-with_ = a => b => a.startsWith(b) #-}

postulate parse-json : String → Maybe JSON
{-# COMPILE JS parse-json = input => {
    try {
        return JSON.parse(input);
    } catch (_e) {
        return undefined;
    }
} #-}

postulate string-slice : ℕ → String → String
{-# COMPILE JS string-slice = n => s => s.slice(Number(n)) #-}

find-map : {A B : Set} → (A → Maybe B) → List A → Maybe B
find-map p [] = nothing
find-map p (a ∷ as) = case p a of λ where
    (just b) → just b
    nothing → find-map p as

parse-kind : JSON → Maybe String
parse-kind (j-object kvs) = find-map (λ where
    ("kind" , j-string kind) → just kind
    _ → nothing) kvs
parse-kind _ = nothing

data AgdaResponse : Set where
    DisplayInfo :
          (errors : List JSON)
        → (invisible-goals : List JSON)
        → (visible-goals : List JSON)
        → (warning : List JSON)
        → AgdaResponse

find-key : String → JSON → Maybe JSON
find-key k (j-object kvs) = find-map (λ (k' , v) → if primStringEquality k k' then just v else nothing) kvs
find-key _ _ = nothing

parse-DisplayInfo : JSON → Maybe AgdaResponse
parse-DisplayInfo o = find-key "info" o >>= λ json → do
    j-array errors ← find-key "errors" json where _ → nothing
    j-array invisible-goals ← find-key "invisibleGoals" json where _ → nothing
    j-array visible-goals ← find-key "visibleGoals" json where _ → nothing
    j-array warnings ← find-key "warnings" json where _ → nothing
    just (DisplayInfo errors invisible-goals visible-goals warnings)
    where
        _>>=_ : {A B : Set} → Maybe A → (A → Maybe B) → Maybe B
        nothing >>= a2mb = nothing
        (just a) >>= a2mb = a2mb a

parse-response : String → Maybe AgdaResponse
parse-response response = do
    let truncated-response = if response starts-with "JSON> " then string-slice 6 response else response
    json-response ← parse-json truncated-response
    parse-kind json-response >>= λ where
        "DisplayInfo" → parse-DisplayInfo json-response
        _ → nothing
    where
        _>>=_ : {A B : Set} → Maybe A → (A → Maybe B) → Maybe B
        nothing >>= a2mb = nothing
        (just a) >>= a2mb = a2mb a