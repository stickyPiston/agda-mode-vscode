# How to use the Agda JS FFI

Some builtin Agda modules already have pragma directives that have efficient JS representations. For example,
`Nat`s are compiled to `number`s, `Bool`s to `boolean`s, `IO` is compiled to CPS, etc.

```agda
module JsFFI where

open import Agda.Primitive using (Level ; _⊔_)

open import Agda.Builtin.Nat
open import Agda.Builtin.Bool
open import Agda.Builtin.String
open import Agda.Builtin.List
open import Agda.Builtin.IO
```

## Regular functions

To define functions defined in JS, you can postulate the function's type and use the COMPILE JS pragma
to define what function should be called instead. This function receives all parameters of the postulated
type, even the implicit ones (which is something to be careful about when using `private variable`s).

```agda
postulate process-api vscode-api Process : Set
postulate child-process : process-api

postulate spawn : process-api → String → List String → Process
{-# COMPILE JS spawn = process => cmd => args => process.spawn(cmd, args) #-}

proc = spawn child-process "agda" ("--interaction-json" ∷ [])
```

## Data types

We can also define the representation of a data type used in the compiled agda code using
pragmas. Firstly, we define the data type in Agda, then we define pragmas for

- each constructor: these are functions that take parameters for each field they contain and
  produce the representation.
- the type: which is used as the pattern matching function. This function receives two arguments:
  the representation and a dictionary that contains entries for each constructor that call the
  corresponding arms of the pattern match. The pattern matching function should be parenthesised since
  agda will inline it when compiling pattern matches.

```agda
data Maybe (A : Set) : Set where
    nothing : Maybe A
    just    : A → Maybe A

{-# COMPILE JS Maybe   = ((x, v) => x === undefined ? v["nothing"]() : v["just"](x)) #-}
{-# COMPILE JS nothing = undefined #-}
{-# COMPILE JS just    = a => a #-}

m₁ m₂ : Maybe Nat
m₁ = just 20
m₂ = nothing
```

## Records 

Records are similar to data types, but you need to additionally define a pragma for every field.
The names of the variables is `<record name>.<field name>` and their only parameter is the constructor
representation. Agda takes care of record updates, it compiles them to a new instance of the record
where the unchanged fields are passed via the field function, and the changed fields are transformed
according to the record update.

```agda
record Σ {ℓ₁ ℓ₂ : Level} (A : Set ℓ₁) (B : A → Set ℓ₂) : Set (ℓ₁ ⊔ ℓ₂) where
    constructor _,_
    field
        proj₁ : A
        proj₂ : B proj₁

{-# COMPILE JS Σ = (([a, b], v) => v["_,_"](a, b)) #-}
{-# COMPILE JS _,_ = a => b => [a, b] #-}
{-# COMPILE JS Σ.proj₁ = ([a, b]) => a #-}
{-# COMPILE JS Σ.proj₂ = ([a, b]) => b #-}

Σ₁ Σ₂ : Σ Nat λ _ → Nat
Σ₁ = 10 , 20
Σ₂ = record Σ₁ { proj₂ = 30 }
```

You can also remove the constructor to make a values of a type only constructable through the ffi. The
representation of this type is then defined by how you define the functions for the fields.

```agda
record System : Set where field
    process : process-api
    vscode  : vscode-api

-- The representation of a System is an object with two properties: "process" and "vscode"
{-# COMPILE JS System.process = ({ process }) => process #-}
{-# COMPILE JS System.vscode  = ({ vscode  }) => vscode #-}
```