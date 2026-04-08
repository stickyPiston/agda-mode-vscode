module Data.Monoid where

open import Level

private variable
  a : Level
  A : Set a

record Semigroup (A : Set a) : Set a where field
  _<>_ : A → A → A

record Monoid (A : Set a) : Set a where
  field
    semigroup : Semigroup A
    empty : A

  open Semigroup semigroup public
