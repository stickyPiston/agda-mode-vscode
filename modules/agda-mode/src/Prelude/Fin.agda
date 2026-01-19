module Prelude.Fin where

open import Prelude.Nat

data Fin : ℕ → Set where
    zero : ∀ {n} → Fin (suc n)
    suc : ∀ {n} → Fin n → Fin (suc n)

_↑ : ∀ {n} → Fin n → Fin (suc n)
zero ↑ = zero
suc n ↑ = suc (n ↑)

to-Fin : (n : ℕ) → Fin (suc n)
to-Fin Prelude.Nat.zero = zero
to-Fin (suc n) = suc (to-Fin n)

data _≤_ : ℕ → ℕ → Set where
    z≤n : ∀ {n} → 0 ≤ n
    sn≤sm : ∀ {n m} → n ≤ m → suc n ≤ suc m

weaken-Fin : ∀ {n m} → n ≤ m → Fin n → Fin m
weaken-Fin (sn≤sm n≤m) zero = zero
weaken-Fin (sn≤sm n≤m) (suc n) = suc (weaken-Fin n≤m n)