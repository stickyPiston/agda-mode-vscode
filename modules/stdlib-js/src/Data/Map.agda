module Data.Map where

open import Data.String
open import Data.Maybe
open import Data.List using (List ; foldr)
open import Data.Product
open import Function

-- Gotta be careful w immutability here

module StringMap where
  postulate t : Set → Set
  {-# POLARITY t ++ #-}

  postulate empty : ∀ {V} → t V
  {-# COMPILE JS empty = _ => ({}) #-}

  postulate insert : ∀ {V} → String → V → t V → t V
  {-# COMPILE JS insert = _ => k => v => o => ({ ...o, [k]: v }) #-}

  postulate combine : ∀ {V} → t V → t V → t V
  {-# COMPILE JS combine = _ => a => b => ({ ...b, ...a }) #-}

  from : ∀ {V} → List (String × V) → t V
  from = foldr empty (flip (uncurry insert))

postulate _!?_ : ∀ {V} → StringMap.t V → String → Maybe V
{-# COMPILE JS _!?_ = _ => o => k => (o.hasOwnProperty(k) ? (a => a["just"](o[k])) : (a => a["nothing"]())) #-}

_[_]:=_ : ∀ {V} → StringMap.t V → String → V → StringMap.t V
o [ k ]:= v = StringMap.insert k v o

_↦_ : ∀ {V} → String → V → StringMap.t V
k ↦ v = StringMap.empty [ k ]:= v

_[_]%=_ : ∀ {V} → StringMap.t V → String → (Maybe V → V) → StringMap.t V
o [ k ]%= f = o [ k ]:= f (o !? k)

infixl 15 _<>_
_<>_ : ∀ {V} → StringMap.t V → StringMap.t V → StringMap.t V
_<>_ = StringMap.combine
