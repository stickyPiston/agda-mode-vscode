module Data.JSON where

open import Iepje.Internal.JS.Language.PrimitiveTypes
open import Data.Map

open import Data.List
open import Data.Maybe
open import Data.String
open import Agda.Builtin.Equality

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
    j-object : StringMap.t JSON → JSON

{-# COMPILE JS JSON = ((x, v) =>
      x === null             ? v["j-null"]()
    : typeof x === "string"  ? v["j-string"](x)
    : typeof x === "boolean" ? v["j-bool"](x)
    : typeof x === "number"  ? v["j-number"](x)
    /* Recursive patterns on the list and the object kvs are applied by the compiler */
    : Array.isArray(x)       ? v["j-array"](x)
                             : v["j-object"](x))
    #-}
{-# COMPILE JS j-null = null #-}
{-# COMPILE JS j-string = s => String(s) #-}
{-# COMPILE JS j-bool = b => Boolean(b) #-}
{-# COMPILE JS j-number = n => Number(n) #-}
{-# COMPILE JS j-array = l => [...l] #-}
{-# COMPILE JS j-object = kvs => kvs #-}

postulate parse-json : String → Maybe JSON
{-# COMPILE JS parse-json = input => {
    try {
        const o = JSON.parse(input);
        return a => a["just"](o);
    } catch (_e) {
        return a => a["nothing"]();
    }
} #-}

record Cloneable (A : Set) : Set where field
    encode : A → JSON 
    decode : JSON → Maybe A
    encode-decode-dual : ∀ a → decode (encode a) ≡ just a
open Cloneable ⦃ ... ⦄ public
