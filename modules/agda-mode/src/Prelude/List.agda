module Prelude.List where

open import Agda.Builtin.List public
open import Iepje.Internal.Utils using (map ; for) public
open import Iepje.Internal.JS.Language.IO
open import Agda.Primitive

private variable
    ℓ₁ ℓ₂ : Level
    A : Set ℓ₁
    B : Set ℓ₂

pattern [_] x = x ∷ []

_++_ : List A → List A → List A
[] ++ bs = bs
(x ∷ as) ++ bs = x ∷  (as ++ bs)

foldr : (A → B → B) → B → List A → B
foldr f b [] = b
foldr f b (a ∷ as) = f a (foldr f b as)

concat : List (List A) → List A
concat = foldr _++_ []

concat-map : (A → List B) → List A → List B
concat-map f as = concat (map f as)

foldrM : (A → B → IO B) → B → List A → IO B
foldrM f b [] = pure b
foldrM f b (a ∷ as) = foldrM f b as >>= f a