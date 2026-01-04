module Prelude.Vec where

open import Agda.Primitive using (Level ; _⊔_)

open import Prelude.Sigma
open import Prelude.Nat using (ℕ ; suc ; _-_)
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

map : ∀ {ℓ₁ ℓ₂ n} {A : Set ℓ₁} {B : Set ℓ₂} → (A → B) → Vec A n → Vec B n
map f [] = []
map f (x ∷ xs) = f x ∷  map f xs

for  : ∀ {ℓ₁ ℓ₂ n} {A : Set ℓ₁} {B : Set ℓ₂} → Vec A n → (A → B) → Vec B n
for xs f = map f xs

postulate to-list : {A : Set} {n : ℕ} → Vec A n → List A
{-# COMPILE JS to-list = _ => _ => as => as #-}

-- TODO: Make n and m Fin k
postulate slice : ∀ {A : Set} {k} → Vec A k → (n : ℕ) → (m : ℕ) → Vec A (m - n)
{-# COMPILE JS slice = _ => _ => l => n => m => l.slice(Number(n), Number(m)) #-}

postulate vec-init : ∀ {A : Set} {n} → Vec A (suc n) → Vec A n
{-# COMPILE JS vec-init = _ => _ => xs => xs.slice(0, xs.length - 1) #-}

postulate last : ∀ {A : Set} {n : ℕ} → Vec A (suc n) → A
{-# COMPILE JS last = _ => _ => l => l[l.length - 1] #-}

unsnoc : ∀ {A : Set} {n : ℕ} → Vec A (suc n) → (Vec A n × A)
unsnoc xs = vec-init xs , last xs

data Fin : ℕ → Set where
    zero : ∀ {n} → Fin (suc n)
    suc : ∀ {n} → Fin n → Fin (suc n)

_↑ : ∀ {n} → Fin n → Fin (suc n)
zero ↑ = zero
suc n ↑ = suc (n ↑)

_!!_ : ∀ {ℓ n} {A : Set ℓ} → Vec A n → Fin n → A
(x ∷ _) !! zero = x
(_ ∷ xs) !! suc i = xs !! i

to-Fin : (n : ℕ) → Fin (suc n)
to-Fin Prelude.Nat.zero = zero
to-Fin (suc n) = suc (to-Fin n)

enumerate-Fin : (n : ℕ) → Vec (Fin n) n
enumerate-Fin Prelude.Nat.zero = []
enumerate-Fin (suc n) = to-Fin n ∷ map (_↑) (enumerate-Fin n)