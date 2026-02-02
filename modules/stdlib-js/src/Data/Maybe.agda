module Data.Maybe where

open import Agda.Primitive
open import Agda.Builtin.Maybe public

private variable
  ℓ ℓ₁ ℓ₂ : Level
  A B C : Set ℓ
  F : Set ℓ₁ → Set ℓ₂

from-Maybe : A → Maybe A → A
from-Maybe b nothing  = b
from-Maybe _ (just a) = a

