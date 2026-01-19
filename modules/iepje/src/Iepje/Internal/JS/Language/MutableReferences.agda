
-- Library for mutable references

-- * Backed by JS variables
-- * Accessible only in IO

module Iepje.Internal.JS.Language.MutableReferences where

open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.Utils

open import Agda.Builtin.Unit

-- Type of mutable references
postulate Ref : ∀ {ℓ} → Set ℓ → Set ℓ

-- Implemented in JS as an object with a single, mutable field, `val`

-- Create a new mutable reference
postulate new : ∀ {ℓ} {A : Set ℓ} → A → IO (Ref A)
{-# COMPILE JS new = _ => _ => a => krefa => krefa ({val : a}) #-}

-- Update the value stored in a mutable reference & return the new value
postulate modify : ∀ {ℓ} {A : Set ℓ} → Ref A → (A → A) → IO A
{-# COMPILE JS modify = _ => _ => refa => f => kt => kt(refa.val = f(refa.val)) #-}

modify-⊤ : ∀ {ℓ} {A : Set ℓ} → Ref A → (A → A) → IO ⊤
modify-⊤ ref f = do
  modify ref f
  pure tt

-- Derived helper functions
set : ∀ {ℓ} {A : Set ℓ} → Ref A → A → IO ⊤
set r a = do
  modify r λ _ → a
  pure tt

get : ∀ {ℓ} {A : Set ℓ} → Ref A → IO A
get r = modify r λ x → x
