module Effect.Monad where

open import Agda.Primitive
open import Effect.Applicative
open import Function

private variable
  ℓ₁ ℓ₂ ℓ₃ : Level
  A : Set ℓ₁
  B : Set ℓ₂
  C : Set ℓ₃

record Monad (M : Set ℓ₁ → Set ℓ₂) : Set (lsuc (ℓ₁ ⊔ ℓ₂)) where
  infixl 1 _>>=_ _>>_ _>=>_
  infixr 1 _=<<_ _<=<_

  field
    applicative : Applicative M
    _>>=_ : M A → (A → M B) → M B

  open Applicative applicative public

  _=<<_ : (A → M B) → M A → M B
  _=<<_ = flip _>>=_

  _>>_ : M A → M B → M B
  _>>_ = _*>_

  _>=>_ : (A → M B) → (B → M C) → A → M C
  (f >=> g) a = f a >>= g

  _<=<_ : (B → M C) → (A → M B) → A → M C
  _<=<_ = flip _>=>_

record MonadPlus (M : Set ℓ₁ → Set ℓ₂) : Set (lsuc (ℓ₁ ⊔ ℓ₂)) where
  field
    monad : Monad M
    alternative : Alternative M

  open Monad monad public
  open Alternative alternative public
