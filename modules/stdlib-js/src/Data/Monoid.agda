module Data.Monoid where

open import Level

private variable a : Level

record Semigroup (A : Set a) : Set a where field
  _<>_ : A → A → A
open Semigroup public

record Monoid (A : Set a) : Set a where field
  semigroup : Semigroup A
  empty : A
open Monoid public
