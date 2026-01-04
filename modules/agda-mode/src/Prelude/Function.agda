module Prelude.Function where

open import Agda.Primitive

private variable
    ℓ₁ ℓ₂ ℓ₃ : Level
    A : Set ℓ₁
    B : Set ℓ₂
    C : Set ℓ₃

const : A → B → A
const a _ = a

_∘_ : (B → C) → (A → B) → A → C
f ∘ g = λ x → f (g x)

_|>_ : A → (A → B) → B
x |> f = f x

infixl 0 _|>_