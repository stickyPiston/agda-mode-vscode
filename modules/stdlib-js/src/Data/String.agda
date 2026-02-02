module Data.String where

open import Agda.Builtin.String
  using (String)
  renaming
    ( primStringEquality to _==_
    ) public

open import Agda.Builtin.String using (primStringAppend)
open import Agda.Builtin.Bool

infixl 10 _++_
_++_ : String → String → String
_++_ = primStringAppend

open import Data.List
open import Agda.Builtin.Nat

postulate _starts-with_ : String → String → Bool
{-# COMPILE JS _starts-with_ = s => pre => s.startsWith(pre) #-}

postulate slice : Nat → String → String
{-# COMPILE JS slice = n => s => s.slice(Number(n)) #-}

postulate lines : String → List String
{-# COMPILE JS lines = xs => xs.split("\n") #-}
