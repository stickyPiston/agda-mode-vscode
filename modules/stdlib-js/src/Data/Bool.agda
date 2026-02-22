module Data.Bool where

open import Agda.Builtin.Bool renaming (Bool to 𝔹) public
open import Level

private variable
  ℓ : Level
  A B : Set ℓ

infixr 2 if_then_else_

if_then_else_ : 𝔹 → A → A → A
if true then t else _ = t
if false then _ else e = e

not : 𝔹 → 𝔹
not true = false
not false = true

infixl 7 _∧_ _∨_

_∧_ : 𝔹 → 𝔹 → 𝔹
false ∧ b = false
_ ∧ b = b

_∨_ : 𝔹 → 𝔹 → 𝔹
true ∨ b = true
_ ∨ b = b
