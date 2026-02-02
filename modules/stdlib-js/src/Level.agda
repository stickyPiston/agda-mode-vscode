module Level where

open import Agda.Primitive public

record Lift {a} ℓ (A : Set a) : Set (a ⊔ ℓ) where
  constructor lift
  field lower : A
open Lift public
