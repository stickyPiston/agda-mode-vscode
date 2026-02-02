
-- Implementation of the IO monad, for use with Agda's JS backend

module Iepje.Internal.JS.Language.IO where

open import Agda.Primitive using (lsuc)

-- This module deliberately does *not*
--
-- import Agda.Builtin.IO
--
-- Instead, IO is postualted, since...
-- * Agda.Builtin.IO.IO might be used by other libraries
-- * Which might use another JS-side implementation
-- * defining `IO = Agda.Builtin.IO.IO` would give users a false sense of compatability
-- * If compatability is desired, users can postulate conversion functions themselves
postulate IO : ∀ {ℓ} → Set ℓ → Set ℓ

-- IO is implemented in CPS because...
--  * CPS prevents execution at definition-time
--  * Agda's JS backend seems to expect CPS (it calls main with (a => {}))

postulate pure : ∀ {ℓ} {A : Set ℓ} → A → IO A
{-# COMPILE JS pure = _ => _ => a => ka => ka(a) #-}

infixl 24 _>>=_
postulate _>>=_ : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} → IO A → (A → IO B) → IO B

-- This implementation generates a JS call stack with depth O(total number of _>>=_ evalauted)
--  {-# COMPILE JS _>>=_ = _ => _ => _ => _ => ma => a2mb => kb => ma(a => a2mb(a)(b => kb(b))) #-}

-- This implementation generates a JS call stack with depth O(max nesting depth of _>>=_)
{-# COMPILE JS _>>=_ = _ => _ => _ => _ => ma => a2mb => kb =>
  {
    let ar;
    ma(a => ar = a);
    let br;
    a2mb(ar)(b => br = b);
    return kb(br);
  }
#-}
