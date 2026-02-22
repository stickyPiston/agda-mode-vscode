module Data.Nat where

open import Agda.Builtin.Nat
open import Data.Bool

ℕ : Set
ℕ = Nat

_>_ : ℕ → ℕ → 𝔹
a > zero = true
zero > b = false
suc a > suc b = a > b
{-# COMPILE JS _>_ = a => b => a > b #-}

_≤_ : ℕ → ℕ → 𝔹
a ≤ b = not (a > b)
