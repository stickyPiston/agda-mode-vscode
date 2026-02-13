module Effect.Applicative where

open import Agda.Primitive
open import Effect.Functor
open import Function

private variable
  ℓ₁ ℓ₂ : Level
  A B C : Set ℓ₁

record Applicative (F : Set ℓ₁ → Set ℓ₂) : Set (lsuc (ℓ₁ ⊔ ℓ₂)) where
  infixl 8 _<*>_

  field
    functor : Functor F
    pure : A → F A
    _<*>_ : F (A → B) → F A → F B

  open Functor functor public

  lift2 : (A → B → C) → F A → F B → F C
  lift2 f a b = f <$> a <*> b

  infixl 8 _<*_

  _<*_ : F A → F B → F A
  _<*_ = lift2 const

  _*>_ : F A → F B → F B
  a *> b = (λ _ b → b) <$> a <*> b

record Alternative (F : Set ℓ₁ → Set ℓ₂) : Set (lsuc (ℓ₁ ⊔ ℓ₂)) where
  infixr 6 _<|>_

  field
    applicative : Applicative F
    ⊘ : F A
    _<|>_ : F A → F A → F A
