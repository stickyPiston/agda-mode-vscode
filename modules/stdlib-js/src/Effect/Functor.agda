module Effect.Functor where

open import Agda.Primitive
open import Function

private variable
  ℓ₁ ℓ₂ : Level
  A B : Set ℓ₁

record Functor (F : Set ℓ₁ → Set ℓ₂) : Set (lsuc (ℓ₁ ⊔ ℓ₂)) where
  field
    fmap : (A → B) → F A → F B

  infixl 9 _<$>_
  _<$>_ : (A → B) → F A → F B
  _<$>_ = fmap

  _<$_ : B → F A → F B
  b <$ f = const b <$> f

  _<&>_ : F A → (A → B) → F B
  _<&>_ = flip _<$>_
