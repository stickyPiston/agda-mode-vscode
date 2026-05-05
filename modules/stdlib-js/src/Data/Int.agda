module Data.Int where

import Data.Nat as Nat
open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.Int public
open import Data.Bool
open import Agda.Builtin.Equality

_+_ : Int → Int → Int
pos n + pos m = pos (n Nat.+ m)
negsuc n + pos m = if m Nat.≤ n then negsuc (n Nat.- m Nat.+ 1) else pos (m Nat.- n Nat.- 1)
pos n + negsuc m = negsuc m + pos n
negsuc n + negsuc m = negsuc (n Nat.+ m)

_ : pos 10 + pos 20 ≡ pos 30
_ = refl

_ : pos 10 + negsuc 4 ≡ pos 5
_ = refl

-_ : Int → Int
- (pos Nat.zero) = pos Nat.zero
- (pos (Nat.suc n)) = negsuc n
- (negsuc n) = pos n

_⊝_ : Nat → Nat → Int
l ⊝ r = if l Nat.≥ r then pos (l Nat.- r) else negsuc (r Nat.- l Nat.- 1)

_ : 1 ⊝ 1 ≡ pos 0
_ = refl
