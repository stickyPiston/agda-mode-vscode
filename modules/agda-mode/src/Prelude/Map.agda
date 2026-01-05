module Prelude.Map where

open import Prelude.List
open import Prelude.Maybe
open import Prelude.String
open import Prelude.Function

-- Gotta be careful w immutability here
postulate StringMap : Set → Set
{-# POLARITY StringMap ++ #-}

postulate empty : ∀ {V} → StringMap V
{-# COMPILE JS empty = _ => ({}) #-}

postulate insert : ∀ {V} → String → V → StringMap V → StringMap V
{-# COMPILE JS insert = _ => k => v => o => ({ [k]: v, ...o }) #-}

postulate _!?_ : ∀ {V} → StringMap V → String → Maybe V
{-# COMPILE JS _!?_ = _ => o => k => (o.hasOwnProperty(k) ? (a => a["just"](o[k])) : (a => a["nothing"]())) #-}

_[_]:=_ : ∀ {V} → StringMap V → String → V → StringMap V
o [ k ]:= v = insert k v o

_↦_ : ∀ {V} → String → V → StringMap V
k ↦ v = empty [ k ]:= v

_[_]%=_ : ∀ {V} → StringMap V → String → (Maybe V → V) → StringMap V
o [ k ]%= f = o [ k ]:= f (o !? k)

postulate combine : ∀ {V} → StringMap V → StringMap V → StringMap V
{-# COMPILE JS combine = _ => a => b => ({ ...a, ...b }) #-}

_<>_ : ∀ {V} → StringMap V → StringMap V → StringMap V
_<>_ = combine