module Communication where

open import Iepje.Internal.JS.Language.PrimitiveTypes
open import Iepje.Internal.Utils hiding (_×_)

open import Agda.Builtin.List
open import Agda.Builtin.Nat
open import Agda.Builtin.Maybe
open import Agda.Builtin.Equality

data _×_ (A B : Set) : Set where
    _,_ : A → B → A × B

{-# COMPILE JS _×_ = (([a, b], v) => v["_,_"](a, b)) #-}
{-# COMPILE JS _,_ = a => b => [a, b] #-}

-- Using the costructors of the JSON data type, we can encode
-- things into cloneable JS objects. Defining encode and decode
-- functions allows us to communicate more complex data types
-- between the extension and the webview.
data JSON : Set where
    j-null : JSON
    j-string : string → JSON
    j-bool : boolean → JSON
    j-number : number → JSON
    j-array : List JSON → JSON
    j-object : List (string × JSON) → JSON

{-# COMPILE JS JSON = ((x, v) =>
      x === null             ? v["j-null"]()
    : typeof x === "string"  ? v["j-string"](x)
    : typeof x === "boolean" ? v["j-bool"](x)
    : typeof x === "number"  ? v["j-number"](x)
    /* Recursive patterns on the list and the object kvs are applied by the compiler */
    : Array.isArray(x)       ? v["j-array"](x)
                             : v["j-object"](Object.entries(x)))
    #-}
{-# COMPILE JS j-null = null #-}
{-# COMPILE JS j-string = s => String(s) #-}
{-# COMPILE JS j-bool = b => Boolean(b) #-}
{-# COMPILE JS j-number = n => Number(n) #-}
{-# COMPILE JS j-array = l => [...l] #-}
{-# COMPILE JS j-object = kvs => /* Dual of Object.entries(...) */
    kvs.reduce((acc, [k, v]) => ({ ...acc, [k]: v }), {}) #-}

-- Now we can define any Agda data type we want as the
-- message data type...
data WebviewMsg : Set where
    a b : WebviewMsg

-- ...as long as we define encode and decode functions for it.
-- The decode function can make use of plain pattern matching.
record Cloneable (A : Set) : Set where field
    encode : A → JSON 
    decode : JSON → Maybe A
    encode-decode-dual : ∀ a → decode (encode a) ≡ just a
open Cloneable ⦃ ... ⦄ public

instance
    MsgCloneable : Cloneable WebviewMsg 
    MsgCloneable = record
        { encode = λ where
            a → j-array (j-string "a" ∷ [])
            b → j-array (j-string "b" ∷ [])
        ; decode = λ where
            (j-array (j-string "a" ∷ [])) → just a
            (j-array (j-string "b" ∷ [])) → just b
            _ → nothing
        ; encode-decode-dual = λ where
            a → refl
            b → refl
        }
