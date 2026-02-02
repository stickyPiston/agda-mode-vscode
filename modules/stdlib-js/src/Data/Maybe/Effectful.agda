module Data.Maybe.Effectful where

open import Effect.Functor
open import Effect.Applicative
open import Effect.Monad
open import Agda.Primitive
open import Data.Maybe

private variable
  ℓ ℓ₁ ℓ₂ ℓ₃ : Level
  A : Set ℓ₁
  B : Set ℓ₂
  C : Set ℓ₃
  F : Set ℓ₁ → Set ℓ₂

module _ where
  private
    fmap : (A → B) → Maybe A → Maybe B
    fmap f nothing  = nothing
    fmap f (just a) = just (f a)

  instance
    functor : Functor {ℓ} Maybe
    functor = record { fmap = fmap }

  private
    pure : A → Maybe A
    pure = just

    _<*>_ : Maybe (A → B) → Maybe A → Maybe B
    nothing <*> _ = nothing
    _ <*> nothing = nothing
    just f <*> just a = just (f a)

  instance
    applicative : Applicative {ℓ} Maybe
    applicative = record
      { functor = functor
      ; pure = pure
      ; _<*>_ = _<*>_
      }

  private
    _<|>_ : Maybe A → Maybe A → Maybe A
    nothing <|> b = b
    a <|> b = a

  instance
    alternative : Alternative {ℓ} Maybe
    alternative = record
      { applicative = applicative
      ; ⊘ = nothing
      ; _<|>_ = _<|>_
      }

  private
    _>>=_ : Maybe A → (A → Maybe B) → Maybe B
    nothing >>= _ = nothing
    just a >>= f = f a

  instance
    monad : Monad {ℓ} Maybe
    monad = record
      { applicative = applicative
      ; _>>=_ = _>>=_
      }

    monad-plus : MonadPlus {ℓ} Maybe
    monad-plus = record
      { monad = monad
      ; alternative = alternative
      }

module TraversableA (applicative : Applicative F) where
  open Applicative applicative

  sequenceA : Maybe (F A) → F (Maybe A)
  sequenceA (just x) = just <$> x
  sequenceA nothing = pure nothing

  mapA : (A → F B) → Maybe A → F (Maybe B)
  mapA f (just x) = just <$> f x
  mapA f nothing = pure nothing
