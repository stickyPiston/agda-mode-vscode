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

maybe : B → (A → B) → Maybe A → B
maybe b _ nothing  = b
maybe _ f (just a) = f a

open import Data.Monoid hiding (_<>_)

private
  mappend : ⦃ Semigroup A ⦄ → Maybe A → Maybe A → Maybe A
  mappend ⦃ s ⦄ (just a) (just b) = just (a <> b) where open Semigroup s
  mappend _ _ = nothing

instance
  monoid : ⦃ Semigroup A ⦄ → Monoid (Maybe A)
  monoid = let s' = record { _<>_ = mappend } in record { semigroup = s' ; empty = nothing }
