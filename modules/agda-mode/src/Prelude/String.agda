module Prelude.String where

open import Agda.Builtin.String public
open import Iepje.Prelude using (_++_) public
open import Prelude.Sigma
open import Prelude.Nat hiding (_==_)
open import Prelude.Vec using (Vec)
open import Agda.Builtin.Bool

postulate split : String → String → Σ[ n ∈ ℕ ] Vec String (suc n)
{-# COMPILE JS split = input => on => {
    /* Array.prototype.split always returns at least one split even if the input string is empty */
    const splits = input.split(on); 
    return [BigInt(splits.length), splits];
} #-}

_==_ : String → String → Bool
_==_ = primStringEquality