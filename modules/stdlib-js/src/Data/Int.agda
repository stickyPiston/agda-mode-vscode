module Data.Int where

import Data.Nat as Nat
open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.Int public
open import Data.Bool

+_ : Nat → Int
+_ = pos

-_ : Int → Int
- (pos Nat.zero) = pos Nat.zero
- (pos (Nat.suc n)) = negsuc n
- (negsuc n) = pos n

_⊝_ : Nat → Nat → Int
l ⊝ r = if l Nat.≥ r then + (l Nat.- r) else negsuc (r Nat.- l Nat.- 1)

open import Agda.Builtin.Equality

_ : 1 ⊝ 1 ≡ + 0
_ = refl
