module Prelude.JSON.Decode where

open import Agda.Primitive
open import Agda.Builtin.Int
open import Agda.Builtin.Float
open import Agda.Builtin.Bool

open import Prelude.Maybe using (Maybe ; just ; nothing ; traverse ; _>>=_)
import Prelude.Maybe as M
open import Prelude.List using (List)
open import Prelude.String using (String)
open import Prelude.JSON
open import Prelude.Function
open import Prelude.Nat
open import Prelude.Sigma
open import Prelude.Vec
open import Prelude.Map

open import Iepje.Internal.JS.Language.PrimitiveTypes using (number)
open import Iepje.Internal.Utils using (case_of_)

private variable
    ℓ₁ ℓ₂ : Level
    A : Set ℓ₁
    B : Set ℓ₂

Decoder : Set → Set
Decoder A = JSON → Maybe A

succeed : A → Decoder A
succeed = const ∘ just

string : Decoder String
string (j-string s) = just s
string _ = nothing

bool : Decoder Bool
bool (j-bool b) = just b
bool _ = nothing

private postulate round : number → Int
{-# COMPILE JS round = BigInt #-}

int : Decoder Int
int (j-number n) = just (round n)
int _ = nothing

private postulate to-ℕ : number → ℕ
{-# COMPILE JS to-ℕ = BigInt #-}

nat : Decoder ℕ
nat (j-number n) = just (to-ℕ n)
nat _ = nothing

private postulate to-float : number → Float
{-# COMPILE JS to-float = Number #-}

float : Decoder Float
float (j-number n) = just (to-float n)
float _ = nothing 

list : Decoder A → Decoder (List A)
list d (j-array xs) = traverse d xs
list _ _ = nothing

private postulate safe-index : ∀ {ℓ} {A : Set ℓ} → ℕ → List A → Maybe A
{-# COMPILE JS safe-index = _ => _ => idx => xs => a => a["just"](xs[idx]) #-}

index : ℕ → Decoder (List A) → Decoder A
index n d json = d json >>= safe-index n

required : String → Decoder A → Decoder A
required name d (j-object kvs) = kvs !? name >>= d
required _ _ _ = nothing

optional : String → Decoder A → Decoder (Maybe A)
optional name d (j-object kvs) = case kvs !? name of λ where
    (just x) → just M.<$> d x
    nothing → just nothing
optional _ _ _ = nothing

optional-null : String → Decoder A → Decoder (Maybe A)
optional-null name d (j-object kvs) = case kvs !? name of λ where
    (just j-null) → just nothing
    (just x) → just M.<$> d x
    nothing → just nothing
optional-null _ _ _ = nothing

_<$>_ : (A → B) → Decoder A → Decoder B
f <$> d = λ json → f M.<$> d json

_<&>_ : Decoder A → (A → B) → Decoder B
d <&> f = f <$> d

_<*>_ : Decoder (A → B) → Decoder A → Decoder B
f <*> d = λ json → f json M.<*> d json

pure : A → Decoder A
pure = succeed