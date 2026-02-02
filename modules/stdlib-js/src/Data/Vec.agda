module Data.Vec where

open import Level
open import Agda.Builtin.Nat

data Vec {a} (A : Set a) : Nat → Set a where
  [] : Vec A 0
  _∷_ : ∀ {n} → A → Vec A n → Vec A (suc n)

{-# COMPILE JS [] = [] #-}
{-# COMPILE JS _∷_ = _ => x => xs => [x, ...xs]  #-}
