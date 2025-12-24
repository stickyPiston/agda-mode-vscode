module AgdaMode.Common.Communication where

open import Iepje.Internal.JS.Language.PrimitiveTypes
open import Prelude.Sigma using (_×_)
open import Prelude.Maybe
open import Prelude.Nat
open import Iepje.Internal.Utils using (case_of_)

open import Agda.Builtin.List
open import Agda.Builtin.Nat
open import Agda.Builtin.Equality
open import Agda.Builtin.Float
open import AgdaMode.Common.JSON

-- Now we can define any Agda data type we want as the
-- message data type...
data WebviewMsg : Set where
    show-errors : ℕ → WebviewMsg

postulate float2nat : Float → Nat
{-# COMPILE JS float2nat = BigInt #-}

postulate float↔nat : ∀ n → float2nat (primNatToFloat n) ≡ n

cong : {A B : Set} → (f : A → B) → {x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

instance
    MsgCloneable : Cloneable WebviewMsg 
    MsgCloneable = record
        { encode = λ where
            (show-errors n) → j-number (primNatToFloat n)
        ; decode = λ where
            (j-number n) → just (show-errors (float2nat n))
            _ → nothing
        ; encode-decode-dual = λ where
            (show-errors n) → cong (λ x → just (show-errors x)) (float↔nat n)
        }