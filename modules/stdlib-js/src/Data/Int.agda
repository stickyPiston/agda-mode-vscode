module Data.Int where

open import Agda.Builtin.Nat
open import Agda.Builtin.Int public

+_ : Nat → Int
+_ = pos

-_ : Int → Int
- (pos n) = negsuc n
- (negsuc n) = pos n
