module Data.JSON.Decode where

open import Level
open import Agda.Builtin.Int
open import Agda.Builtin.Float
open import Agda.Builtin.Bool

open import Effect.Functor
open import Effect.Monad
open import Data.Maybe
import Data.Maybe.Effectful as Maybe
open import Agda.Builtin.String
open import Agda.Builtin.Nat
open import Data.List
open Data.List.TraversableA
open import Function
open import Agda.Builtin.Unit

open import Data.JSON
import Data.Map as M

open import Iepje.Internal.JS.Language.PrimitiveTypes using (number)

private variable
    ℓ₁ ℓ₂ ℓ : Level
    A B : Set ℓ₁

module _ where
  open Monad ⦃ ... ⦄

  Decoder : Set ℓ → Set ℓ
  Decoder A = JSON → Maybe A

  succeed : A → Decoder A
  succeed a _ = just a

  any : Decoder JSON
  any a = just a

  string : Decoder String
  string (j-string s) = just s
  string _ = nothing

  bool : Decoder Bool
  bool (j-bool b) = just b
  bool _ = nothing

  private postulate round : number → Int
  {-# COMPILE JS round = BigInt #-}

  int : Decoder Int
  int (j-number n) = just (round n)
  int _ = nothing

  private postulate to-Nat : number → Nat
  {-# COMPILE JS to-Nat = BigInt #-}

  nat : Decoder Nat
  nat (j-number n) = just (to-Nat n)
  nat _ = nothing

  private postulate to-float : number → Float
  {-# COMPILE JS to-float = Number #-}

  float : Decoder Float
  float (j-number n) = just (to-float n)
  float _ = nothing

  list : Decoder A → Decoder (List A)
  list d (j-array xs) = mapA Maybe.applicative d xs
  list _ _ = nothing

  private
    _!?_ : Nat → List A → Maybe A
    _!?_ zero (x ∷ _) = just x
    _!?_ (suc n) (_ ∷ xs) = n !? xs
    _!?_ _ _ = nothing

  index : Nat → Decoder (List A) → Decoder A
  index n d json = d json >>= n !?_

  required : String → Decoder A → Decoder A
  required name d (j-object kvs) = case kvs M.!? name of λ where
    (just json) → d json
    nothing → nothing
  required _ _ _ = nothing

  optional : String → Decoder A → Decoder (Maybe A)
  optional name d (j-object kvs) = case kvs M.!? name of λ where
      (just x) → just <$> d x
      nothing → just nothing
  optional _ _ _ = nothing

  optional-null : String → Decoder A → Decoder (Maybe A)
  optional-null name d (j-object kvs) = case kvs M.!? name of λ where
      (just j-null) → just nothing
      (just x) → just <$> d x
      nothing → just nothing
  optional-null _ _ _ = nothing

-- Effects

open import Effect.Applicative
open import Effect.Monad

private
  fmap : {A B : Set ℓ} → (A → B) → Decoder A → Decoder B
  fmap f d json = f <$> d json
      where open Functor Maybe.functor

  _<*>'_ : {A B : Set ℓ} → Decoder (A → B) → Decoder A → Decoder B
  f <*>' d = λ json → f json <*> d json
      where open Applicative Maybe.applicative

instance
  functor : Functor {ℓ} Decoder
  functor = record { fmap = fmap }

  applicative : Applicative {ℓ} Decoder
  applicative = record { functor = functor ; pure = succeed ; _<*>_ = _<*>'_ }

_>>='_ : {A B : Set ℓ} → Decoder A → (A → Decoder B) → Decoder B
d >>=' f = λ json → d json >>= λ a → (f a) json
  where open Monad Maybe.monad

instance
  monad : Monad {ℓ} Decoder
  monad = record
    { applicative = applicative
    ; _>>=_ = _>>='_
    }

_<|>'_ : Decoder A → Decoder A → Decoder A
a <|>' b = λ json → a json <|> b json
  where open Alternative Maybe.alternative

instance
  alternative : Alternative {ℓ} Decoder
  alternative = record
    { _<|>_ = _<|>'_
    ; ⊘ = const nothing
    ; applicative = applicative
    }

  monad-plus : MonadPlus {ℓ} Decoder
  monad-plus = record { monad = monad ; alternative = alternative }
