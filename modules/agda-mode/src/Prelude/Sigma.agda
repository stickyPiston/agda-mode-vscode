module Prelude.Sigma where

open import Agda.Primitive using (Level ; _⊔_)

private variable
    ℓ ℓ₁ ℓ₂ : Level

record Σ (A : Set ℓ₁) (B : A → Set ℓ₂) : Set (ℓ₁ ⊔ ℓ₂) where
    constructor _,_
    field
        proj₁ : A
        proj₂ : B proj₁

{-# COMPILE JS Σ = (([a, b], v) => v["_,_"](a, b)) #-}
{-# COMPILE JS _,_ = a => b => [a, b] #-}
{-# COMPILE JS Σ.proj₁ = ([a, b]) => a #-}
{-# COMPILE JS Σ.proj₂ = ([a, b]) => b #-}

infix 2 Σ-syntax

Σ-syntax : (A : Set ℓ₁) → (A → Set ℓ₂) → Set (ℓ₁ ⊔ ℓ₂)
Σ-syntax = Σ

syntax Σ-syntax A (λ x → B) = Σ[ x ∈ A ] B

infix 3 _,,_
record _×_ (A : Set ℓ₁) (B : Set ℓ₂) : Set (ℓ₁ ⊔ ℓ₂) where
    constructor _,,_
    field
        fst : A
        snd : B
open _×_ public