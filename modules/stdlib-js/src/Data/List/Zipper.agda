module Data.List.Zipper where

open import Level
open import Data.List hiding (null?)
open import Data.Maybe
open import Data.Bool

private variable
  a : Level
  A : Set a

module Zipper where
  record t (A : Set a) : Set a where field
    right rev-left : List A

  go-left go-right : t A → t A
  go-left record { right = right ; rev-left = [] } =
    record { right = right ; rev-left = [] }
  go-left record { right = right ; rev-left = (x ∷ rev-left) } =
    record { right = x ∷ right ; rev-left = rev-left }

  go-right record { right = [] ; rev-left = rev-left } =
    record { right = [] ; rev-left = rev-left }
  go-right record { right = (x ∷ right) ; rev-left = rev-left } =
    record { right = right ; rev-left = x ∷ rev-left }

  focus : t A → Maybe A
  focus record { right = [] } = nothing
  focus record { right = (x ∷ _) } = just x

  map-focus : (A → A) → t A → t A
  map-focus f z@record { right = (x ∷ xs) } = record z { right = f x ∷ xs }
  map-focus f z = z

  from-list : List A → t A
  from-list l = record { rev-left = [] ; right = l }

  to-list : t A → List A
  to-list record { right = right ; rev-left = rev-left } = reverse rev-left ++ right

  null? : t A → 𝔹
  null? record { right = [] ; rev-left = [] } = true
  null? _ = false

  open import Agda.Builtin.Equality

  to-from-list-dual : (l : List A) → to-list (from-list l) ≡ l
  to-from-list-dual l = refl
