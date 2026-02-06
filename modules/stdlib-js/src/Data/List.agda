module Data.List where

open import Agda.Primitive
open import Agda.Builtin.List public
open import Agda.Builtin.Nat
open import Function
open import Data.Maybe
open import Data.Product

private variable
  a b : Level
  A : Set a
  B : Set b
  F M : Set a → Set b

pattern [_] a = a ∷ []

fold : B → (B → A → B) → List A → B
fold b f [] = b
fold b f (x ∷ xs) = fold (f b x) f xs

{-# COMPILE JS fold = a => A => b => B => b => f => xs => xs.reduceRight((ac, e) => f(ac)(e), b) #-}

postulate _times_ : Nat → (Nat → A) → List A
{-# COMPILE JS _times_ = a => A => n => f => Array(Number(n)).fill(null).map((_, i) => f(BigInt(i))) #-}

map : (A → B) → List A → List B
map f = fold [] λ ac x → f x ∷ ac

_++_ : List A → List A → List A
[] ++ b = b
(x ∷ a) ++ b = x ∷ (a ++ b)

{-# COMPILE JS _++_ = a => A => l => r => l.concat(r) #-}

concat : List (List A) → List A
concat = fold [] λ ac l → ac ++ l

unsnoc : List A → Maybe (List A × A)
unsnoc [] = nothing
unsnoc (x ∷ []) = just ([] , x)
unsnoc (a ∷ as) = case unsnoc as of λ where
  (just (as , b)) → just (a ∷ as , b)
  nothing → nothing

{-# COMPILE JS unsnoc = a => A => xs => {
  if (xs.length) {
    return x => x["just"]({ "_,_": y => y["_,_"](xs.slice(0, xs.length - 1), xs[xs.length - 1]) });
  } else {
    return x => x["nothing"]();
  }
} #-}

open import Effect.Applicative

module TraversableA (applicative : Applicative F) where
  open Applicative applicative

  sequenceA : List (F A) → F (List A)
  sequenceA = fold (pure []) λ ac x → (_∷_ <$> x) <*> ac

  mapA : (A → F B) → List A → F (List B)
  mapA = sequenceA ∘₂ map

open import Effect.Monad

module TraversableM (monad : Monad M) where
  open Monad monad

  mapM : (A → M B) → List A → M (List B)
  mapM f = fold (pure []) λ ac a → f a >>= λ b → (b ∷_) <$> ac

  forM : List A → (A → M B) → M (List B)
  forM = flip mapM
