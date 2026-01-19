module Prelude.Maybe where

open import Agda.Primitive using (Level)
open import Prelude.List
open List using ([] ; _∷_)
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

justs : List.t (Maybe A) → List.t A
justs [] = []
justs (nothing ∷ as) = justs as
justs (just a ∷ as) = a ∷ justs as

map-maybe : (A → Maybe B) → List.t A → List.t B
map-maybe f as = justs (List.map f as)

traverse : (A → Maybe B) → List.t A → Maybe (List.t B)
traverse f [] = just []
traverse f (a ∷ as) = ⦇ f a ∷ traverse f as ⦈

traverse-Vec : (A → Maybe B) → Vec A n → Maybe (Vec B n)
traverse-Vec f [] = just []
traverse-Vec f (a ∷ as) = ⦇ f a ∷ traverse-Vec f as ⦈

foldM : B → List.t A → (B → A → Maybe B) → Maybe B
foldM b [] f = just b
foldM b (a ∷ as) f = do
    b' ← f b a
    foldM b' as f

find-Vec : (A → Bool) → Vec A n → Maybe A
find-Vec p [] = nothing
find-Vec p (x ∷ xs) = if p x then just x else find-Vec p xs

find : (A → Bool) → List.t A → Maybe A
find p [] = nothing
find p (x ∷ xs) = if p x then just x else find p xs

maybe : (A → B) → B → Maybe A → B
maybe _ b nothing  = b
maybe f _ (just x) = f x