module Prelude.Function where

open import Agda.Primitive
open import Agda.Builtin.Bool

private variable
    ℓ₁ ℓ₂ ℓ₃ : Level
    A : Set ℓ₁
    B : Set ℓ₂
    C : Set ℓ₃

id : A → A
id x = x

const : A → B → A
const a _ = a

infixr 10 _∘_
_∘_ : (B → C) → (A → B) → A → C
f ∘ g = λ x → f (g x)

_|>_ : A → (A → B) → B
x |> f = f x

infixl 0 _|>_

_$_ : (A → B) → A → B
f $ x = f x

infixl 0 _$_

if_then_else_ : Bool → A → A → A
if true then t else _ = t
if false then _ else e = e

case_returning_of_ : ∀ {A : Set ℓ₁} (x : A) (B : A → Set ℓ₂) → ((x : A) → B x) → B x
case x returning B of f = f x