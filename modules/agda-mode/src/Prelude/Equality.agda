module Prelude.Equality where

open import Agda.Builtin.Equality public
open import Agda.Primitive

private variable
    ℓ ℓ₁ ℓ₂ : Level

coerce : {A B : Set} → A ≡ B → A → B
coerce refl a = a

cong : {A : Set ℓ₁} {B : Set ℓ₂} {a b : A} → (f : A → B) → a ≡ b → f a ≡ f b
cong _ refl = refl 

sym : ∀ {A B : Set ℓ} → A ≡ B → B ≡ A
sym refl = refl