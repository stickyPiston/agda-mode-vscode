module Iepje.Internal.Effect where

open import Agda.Builtin.List
open import Agda.Builtin.Unit
open import Agda.Builtin.Nat
open import Agda.Primitive

open import Iepje.Internal.JS.Language.IO

private variable msg : Set

record Effect msg : Set where
    field actions : List ((msg → IO ⊤) → IO ⊤)

from : ((dispatch : msg → IO ⊤) → IO ⊤) → Effect msg
from effect = record { actions = effect ∷ [] }

postulate concatMap : ∀ {ℓ : Level} {A B : Set ℓ} → (A → List B) → List A → List B
{-# COMPILE JS concatMap = _ => _ => _ => f => as => as.flatMap(f) #-}

batch : ∀ {A} → List (Effect A) → Effect A
batch effects = record { actions = concatMap Effect.actions effects }

none : ∀ {msg} → Effect msg
none = record { actions = [] }