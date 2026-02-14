module Data.IO where

open import Effect.Functor
open import Effect.Applicative
open import Effect.Monad
open import Level

postulate IO : ∀ {ℓ} → Set ℓ → Set ℓ

module Effectful where
  private variable
      ℓ : Level
      A B : Set ℓ

  private
    postulate pure : ∀ {ℓ} {A : Set ℓ} → A → IO A
    {-# COMPILE JS pure = _ => _ => a => cont => cont(a) #-}

    infixl 8 _>>=_
    postulate _>>=_ : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} → IO A → (A → IO B) → IO B
    {-# COMPILE JS _>>=_ = _ => _ => _ => _ => ma => a2mb => cont => {
      /* let ar; ma(a => ar = a);
      let br; a2mb(ar)(b => br = b);
      return cont(br); */
      ma(a => a2mb(a)(b => cont(b)))
    } #-}

  private
    _<$>_ : (A → B) → IO A → IO B
    _<$>_ f io = io >>= λ a → pure (f a)

  instance
    functor : Functor {ℓ} IO
    functor = record { fmap = _<$>_ }

  private
    _<*>_ : IO (A → B) → IO A → IO B
    _<*>_ io-f io-a = io-f >>= (_<$> io-a)

  instance
    applicative : Applicative {ℓ} IO
    applicative = record
        { functor = functor
        ; pure = pure
        ; _<*>_ = _<*>_ }

  instance
    monad : Monad {ℓ} IO
    monad = record { applicative = applicative ; _>>=_ = _>>=_ }

module Ref where
  open import Function using (_∘_)
  open import Agda.Builtin.Unit

  open Effectful
  open Monad ⦃ ... ⦄

  postulate t : ∀ {ℓ} → Set ℓ → Set ℓ

  postulate new : {ℓ : Level} {A : Set ℓ} → A → IO (t A)
  {-# COMPILE JS new = _ => _ => a => cont => cont({ value: a }) #-}

  postulate get : {ℓ : Level} {A : Set ℓ} → t A → IO A
  {-# COMPILE JS get = _ => _ => ref => cont => cont(ref.value) #-}

  postulate set : {ℓ : Level} {A : Set ℓ} → t A → A → IO (Lift ℓ ⊤)
  {-# COMPILE JS set = _ => _ => ref => a => cont => { ref.value = a; cont(a => a["tt"]()) } #-}

  modify : {ℓ : Level} {A : Set ℓ} → (A → A) → t A → IO (Lift ℓ ⊤)
  modify f ref = get ref >>= set ref ∘ f
