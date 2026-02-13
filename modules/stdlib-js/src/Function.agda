module Function where

open import Agda.Primitive
open import Agda.Builtin.Bool

private variable
  ℓ₁ ℓ₂ : Level
  A B C D : Set ℓ₁

const : A → B → A
const a _ = a

flip : (A → B → C) → (B → A → C)
flip f b a = f a b

_∘_ : (B → C) → (A → B) → A → C
(f ∘ g) a = f (g a)

_∘₂_ : (C → D) → (A → B → C) → A → B → D
(f ∘₂ g) a b = f (g a b)

infixl 1 _|>_

_|>_ : A → (A → B) → B
a |> f = f a

infixl 0 _$_
_$_ : (A → B) → A → B
_$_ = flip _|>_

case_of_ : A → (A → B) → B
case_of_ = _|>_

id : A → A
id a = a

infixl 5 _⟨_⟩_

_⟨_⟩_ : A → (A → B → C) → B → C
a ⟨ f ⟩ b = f a b
