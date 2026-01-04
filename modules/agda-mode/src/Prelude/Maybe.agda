module Prelude.Maybe where

open import Agda.Primitive using (Level)
open import Prelude.List using (List ; map ; [] ; _∷_)
open import Agda.Builtin.String
open import Agda.Builtin.Bool
open import Prelude.Sigma
open import Iepje.Internal.Utils using (if_then_else_)
open import Prelude.Vec using (Vec ; [] ; _∷_)
open import Prelude.Nat

private variable 
    ℓ₁ ℓ₂ : Level
    A : Set ℓ₁
    B : Set ℓ₂
    n : ℕ

data Maybe {ℓ} (A : Set ℓ) : Set ℓ where
    nothing : Maybe A
    just    : A → Maybe A

-- {-# COMPILE JS Maybe   = ((x, v) => x === undefined ? v["nothing"]() : v["just"](x)) #-}
-- {-# COMPILE JS nothing = undefined #-}
-- {-# COMPILE JS just    = x => x #-}

infix 1 _or-else_
_or-else_ : Maybe A → A → A
just x  or-else _ = x
nothing or-else b = b

from-Maybe : A → Maybe A → A
from-Maybe b x = x or-else b

fmap : (A → B) → Maybe A → Maybe B
fmap f nothing = nothing
fmap f (just x) = just (f x)

infix 3 _<$>_
_<$>_ : (A → B) → Maybe A → Maybe B
_<$>_ = fmap

infix 2 _<*>_
_<*>_ : Maybe (A → B) → Maybe A → Maybe B
just f <*> just a = just (f a)
_ <*> _ = nothing

pure : A → Maybe A
pure = just

_>>=_ : Maybe A → (A → Maybe B) → Maybe B
nothing >>= f = nothing
just x >>= f = f x
infixl 10 _>>=_

justs : List (Maybe A) → List A
justs [] = []
justs (nothing ∷ as) = justs as
justs (just a ∷ as) = a ∷ justs as

map-maybe : (A → Maybe B) → List A → List B
map-maybe f as = justs (map f as)

traverse : (A → Maybe B) → List A → Maybe (List B)
traverse f [] = just []
traverse f (a ∷ as) = ⦇ f a ∷ traverse f as ⦈

traverse-Vec : (A → Maybe B) → Vec A n → Maybe (Vec B n)
traverse-Vec f [] = just []
traverse-Vec f (a ∷ as) = ⦇ f a ∷ traverse-Vec f as ⦈

lookup : String → List (String × A) → Maybe A
lookup name [] = nothing
lookup name ((k , v) ∷ kvs) = if primStringEquality name k then just v else lookup name kvs

find-Vec : (A → Bool) → Vec A n → Maybe A
find-Vec p [] = nothing
find-Vec p (x ∷ xs) = if p x then just x else find-Vec p xs

_!?_ : List (String × A) → String  → Maybe A
m !? k = lookup k m