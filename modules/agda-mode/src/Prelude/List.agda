module Prelude.List where

open import Agda.Builtin.List public
open import Iepje.Internal.Utils using (map) public
open import Iepje.Internal.JS.Language.IO

pattern [_] x = x ∷ []

_++_ : ∀ {ℓ} {A : Set ℓ} → List A → List A → List A
[] ++ bs = bs
(x ∷ as) ++ bs = x ∷  (as ++ bs)

foldr : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} → (A → B → B) → B → List A → B
foldr f b [] = b
foldr f b (a ∷ as) = f a (foldr f b as)

concat : ∀ {ℓ} {A : Set ℓ} → List (List A) → List A
concat = foldr _++_ []

concat-map : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} → (A → List B) → List A → List B
concat-map f as = concat (map f as)

foldrM : ∀ {A B : Set} → (A → B → IO B) → B → List A → IO B
foldrM f b [] = pure b
foldrM f b (a ∷ as) = foldrM f b as >>= f a
