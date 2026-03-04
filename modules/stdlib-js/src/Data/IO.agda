module Data.IO where

open import Effect.Functor
open import Effect.Applicative
open import Effect.Monad

open import Data.Bool
open import Agda.Builtin.Unit

open import Level

postulate IO : ∀ {ℓ} → Set ℓ → Set ℓ
-- IO = <A>() => Promise<A>

module Effectful where
  private variable
    ℓ : Level
    A B : Set ℓ

  private
    postulate pure : ∀ {ℓ} {A : Set ℓ} → A → IO A
    -- {-# COMPILE JS pure = _ => _ => a => cont => cont(a) #-}
    {-# COMPILE JS pure = _ => _ => a => async () => a #-}

    infixl 8 _>>=_
    postulate _>>=_ : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} → IO A → (A → IO B) → IO B
    -- {-# COMPILE JS _>>=_ = _ => _ => _ => _ => ma => a2mb => cont => {
    --   let ar; ma(a => ar = a);
    --   let br; a2mb(ar)(b => br = b);
    --   return cont(br);
    -- } #-}
    {-# COMPILE JS _>>=_ = _ => _ => _ => _ => ma => a2mb => async () => {
      const a = await ma();
      return await a2mb(a)();
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

open Effectful
open Monad ⦃ ... ⦄

when : 𝔹 → IO ⊤ → IO ⊤
when true a = a
when false _ = pure tt

unless : 𝔹 → IO ⊤ → IO ⊤
unless false a = a
unless true _ = pure tt

module Ref where
  open import Function using (_∘_)
  open import Agda.Builtin.Unit

  postulate t : Set → Set

  postulate new : {A : Set} → A → IO (t A)
  {-# COMPILE JS new = _ => a => async () => ({ value: a }) #-}

  postulate get : {A : Set} → t A → IO A
  {-# COMPILE JS get = _ => ref => async () => ref.value #-}

  postulate set : {A : Set} → t A → A → IO ⊤
  {-# COMPILE JS set = _ => ref => a => async () => { ref.value = a; return a => a["tt"]() } #-}

  modify : {A : Set} → (A → A) → t A → IO ⊤
  modify f ref = get ref >>= set ref ∘ f
