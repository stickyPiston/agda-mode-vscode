module Prelude.Map where

open import Prelude.List
open import Prelude.Maybe
open import Prelude.String
open import Prelude.Function
open import Agda.Primitive

private variable
    ℓ : Level
    V : Set ℓ

-- Gotta be careful w immutability here
postulate StringMap : Set ℓ → Set ℓ
{-# POLARITY StringMap _ ++ #-}

postulate empty : StringMap V
{-# COMPILE JS empty = _ => _ => ({}) #-}

postulate insert : String → V → StringMap V → StringMap V
{-# COMPILE JS insert = _ => _ => k => v => o => ({ ...o, [k]: v }) #-}

postulate _!?_ : StringMap V → String → Maybe V
{-# COMPILE JS _!?_ = _ => _ => o => k => (o.hasOwnProperty(k) ? (a => a["just"](o[k])) : (a => a["nothing"]())) #-}

_[_]:=_ : StringMap V → String → V → StringMap V
_[_]:=_ {ℓ} {V} o k v = insert {ℓ} {V} k v o

infix 10 _↦_

_↦_ : String → V → StringMap V
_↦_ {ℓ} {V} k v =  _[_]:=_ {ℓ} {V} (empty {ℓ} {V}) k v

_[_]%=_ : StringMap V → String → (Maybe V → V) → StringMap V
_[_]%=_ {ℓ} {V} o k f = _[_]:=_ {ℓ} {V} o k (f (o !? k))

postulate combine : StringMap V → StringMap V → StringMap V
{-# COMPILE JS combine = _ => _ => a => b => ({ ...b, ...a }) #-}

infix 8 _<>_

_<>_ : StringMap V → StringMap V → StringMap V
_<>_ {ℓ} {V} = combine {ℓ} {V}