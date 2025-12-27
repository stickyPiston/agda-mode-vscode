module Prelude.Vec where

open import Agda.Primitive using (Level ; _⊔_)

open import Prelude.Sigma
open import Prelude.Nat using (ℕ ; suc ; _-_)
open import Prelude.Maybe
open import Iepje.Internal.Utils using (case_of_)
open import Agda.Builtin.List using (List)

data Vec {ℓ : Level} (A : Set ℓ) : ℕ → Set ℓ where
    [] : Vec A 0
    _∷_ : ∀ {n : ℕ} → A → Vec A n → Vec A (suc n)

{-# COMPILE JS Vec = ((x, v) => {
    if (x.length > 0) { const [a, ...as] = x; return v["_∷_"](as.length, a, as); }
    else { return v["[]"](); }
}) #-}
{-# COMPILE JS [] = [] #-}
{-# COMPILE JS _∷_ = _ => a => as => [a, ...as] #-}

map-maybe : ∀ {A B : Set} {n} → (A → Maybe B) → Vec A n → Σ[ m ∈ ℕ ] Vec B m
map-maybe f [] = 0 , []
map-maybe f (x ∷ xs) with map-maybe f xs | f x
... | m , xs' | (just x') = suc m , (x' ∷ xs')
... | m , xs' | nothing   = m , xs'

postulate to-list : {A : Set} {n : ℕ} → Vec A n → List A
{-# COMPILE JS to-list = _ => _ => as => as #-}

-- TODO: Make n and m Fin k
postulate slice : ∀ {A : Set} {k} → Vec A k → (n : ℕ) → (m : ℕ) → Vec A (m - n)
{-# COMPILE JS slice = _ => _ => l => n => m => l.slice(Number(n), Number(m)) #-}

vec-init : ∀ {A : Set} {n} → Vec A (suc n) → Vec A n
vec-init {n = n} as = slice as 0 n

postulate last : ∀ {A : Set} {n : ℕ} → Vec A (suc n) → A
{-# COMPILE JS last = _ => _ => l => l[l.length - 1] #-}

unsnoc : ∀ {A : Set} {n : ℕ} → Vec A (suc n) → (Vec A n × A)
unsnoc xs = vec-init xs , last xs
