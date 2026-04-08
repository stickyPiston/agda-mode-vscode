module Data.Nat where

open import Agda.Builtin.String using (primShowNat)
open import Agda.Builtin.Nat public
open import Data.Bool
open import Data.Product

ℕ : Set
ℕ = Nat

_>_ : ℕ → ℕ → 𝔹
zero > b = false
a > zero = true
suc a > suc b = a > b
{-# COMPILE JS _>_ = a => b => a > b #-}

_≤_ : ℕ → ℕ → 𝔹
a ≤ b = not (a > b)

_≥_ : Nat → Nat → Bool
a ≥ zero = true
a ≥ suc b = a > b

infix 4 _≤_ _>_ _≥_

min : ℕ → ℕ → ℕ
min a b = if a ≤ b then a else b

max : ℕ → ℕ → ℕ
max a b = if a ≤ b then b else a

postulate ⌊_/_⌋ : ℕ → ℕ → ℕ
{-# COMPILE JS ⌊_/_⌋ = a => b => a / b #-}

_mod_ : ℕ → ℕ → ℕ
a mod b = a - ⌊ a / b ⌋ * b

_quot-rem_ : ℕ → ℕ → ℕ × ℕ
a quot-rem b = ⌊ a / b ⌋ , a mod b

show = primShowNat
