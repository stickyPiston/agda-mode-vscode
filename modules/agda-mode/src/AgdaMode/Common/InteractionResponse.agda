module AgdaMode.Common.InteractionResponse where

open import Prelude.Maybe
open import Prelude.Nat hiding (_==_)
open import Prelude.Sigma
open import AgdaMode.Common.Communication
open import Prelude.JSON

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
        const o = JSON.parse(input);
        return a => a["just"](o);
    } catch (_e) {
        return a => a["nothing"]();
    }
} #-}

postulate string-slice : ℕ → String → String
{-# COMPILE JS string-slice = n => s => s.slice(Number(n)) #-}

parse-response : String → Maybe JSON
parse-response response = do
    let truncated-response = if response starts-with "JSON> " then string-slice 6 response else response
    parse-json truncated-response