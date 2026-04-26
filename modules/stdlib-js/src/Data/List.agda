module Data.List where

open import Agda.Primitive
open import Agda.Builtin.List public
open import Data.Nat
open import Function
open import Data.Maybe
open import Data.Product
open import Data.Bool
open import Data.Monoid

private variable
  a b c : Level
  A : Set a
  B : Set b
  C : Set c
  F M : Set a → Set b

pattern [_] a = a ∷ []

foldr : B → (B → A → B) → List A → B
foldr b f [] = b
foldr b f (x ∷ xs) = f (foldr b f xs) x

{-# COMPILE JS foldr = a => A => b => B => b => f => xs => xs.reduceRight((ac, e) => f(ac)(e), b) #-}

postulate _times_ : Nat → (Nat → A) → List A
{-# COMPILE JS _times_ = a => A => n => f => Array(Number(n)).fill(null).map((_, i) => f(BigInt(i))) #-}

map : (A → B) → List A → List B
map f = foldr [] λ ac x → f x ∷ ac

for : List A → (A → B) → List B
for = flip map

null? : List A → 𝔹
null? [] = true
null? _ = false

filter : (A → Bool) → List A → List A
filter p [] = []
filter p (x ∷ xs) = if p x then x ∷ filter p xs else filter p xs
{-# COMPILE JS filter = a => A => p => xs => xs.filter(p) #-}

partition : (A → Bool) → List A → List A × List A
partition p xs = filter p xs , filter (not ∘ p) xs
{-# COMPILE JS partition = a => A => p => xs => {
  const t = [], f = [];
  for (const x of xs) {
    (p(x) ? t : f).push(x);
  }
  return { "_,_": y => y["_,_"](t, f) };
} #-}

infixl 10 _++_

_++_ : List A → List A → List A
[] ++ b = b
(x ∷ a) ++ b = x ∷ (a ++ b)

append : List A → List A → List A
append = _++_

{-# COMPILE JS _++_ = a => A => l => r => [...l, ...r] #-}

infix 3 _!!_

_!!_ : List A → Nat → Maybe A
[]     !! _ = nothing
x ∷ _  !! 0 = just x
_ ∷ xs !! n = xs !! n - 1

{-# COMPILE JS _!!_ = _ => _ => xs => n =>
  n < xs.length ? (a => a["just"](xs[n])) : (a => a["nothing"]()) #-}

concat : List (List A) → List A
concat = foldr [] λ ac l → l ++ ac

concat-for : List A → (A → List B) → List B
concat-for = concat ∘₂ for

concat-map : (A → List B) → List A → List B
concat-map = flip concat-for

reverse : List A → List A
reverse [] = []
reverse (x ∷ xs) = reverse xs ++ [ x ]
{-# COMPILE JS reverse = a => A => as => as.toReversed() #-}

take : ℕ → List A → List A
take zero xs = []
take (suc n) [] = []
take (suc n) (x ∷ xs) = x ∷ take n xs
{-# COMPILE JS take = a => A => n => xs => xs.slice(0, Number(n)) #-}

take-while : (A → Bool) → List A → List A
take-while p [] = []
take-while p (x ∷ xs) = if p x then x ∷ take-while p xs else []
{-# COMPILE JS take-while = a => A => p => xs => {
  const i = xs.findIndex(x => !p(x));
  return i === -1 ? xs : xs.slice(0, i);
} #-}

-- TODO: This function is a little dubious, probably needs to be fixed
postulate sort : List A → List A
{-# COMPILE JS sort = _ => _ => xs => xs.toSorted() #-}

postulate sort-on : (A → B) → List A → List A
{-# COMPILE JS sort-on = a => A => b => B => f => as =>
  as.toSorted((x, y) => f(x) - f(y)) #-}

snoc : A → List A → List A
snoc x xs = xs ++ [ x ]
{-# COMPILE JS snoc = a => A => x => xs => [...xs, x] #-}

unsnoc : List A → Maybe (List A × A)
unsnoc [] = nothing
unsnoc (x ∷ []) = just ([] , x)
unsnoc (a ∷ as) = case unsnoc as of λ where
  (just (as , b)) → just (a ∷ as , b)
  nothing → nothing

{-# COMPILE JS unsnoc = a => A => xs => {
  if (xs.length) {
    return x => x["just"]({ "_,_": y => y["_,_"](xs.slice(0, xs.length - 1), xs[xs.length - 1]) });
  } else {
    return x => x["nothing"]();
  }
} #-}

find : (A → 𝔹) → List A → Maybe A
find p [] = nothing
find p (x ∷ xs) = if p x then just x else (find p xs)
{-# COMPILE JS find = a => A => p => xs => {
  const found = xs.find(p);
  return found ? (x => x["just"](found)) : (x => x["nothing"]());
} #-}

∥_∥ : List A → Nat
∥ [] ∥ = 0
∥ _ ∷ xs ∥ = 1 + ∥ xs ∥
{-# COMPILE JS ∥_∥ = _ => _ => xs => BigInt(xs.length) #-}

postulate slice : List A → Nat → Nat → List A
{-# COMPILE JS slice = _ => _ => xs => s => e => xs.slice(Number(s), Number(e)) #-}

zip-with : (A → B → C) → List A → List B → List C
zip-with f [] ys = []
zip-with f xs [] = []
zip-with f (x ∷ xs) (y ∷ ys) = f x y ∷ zip-with f xs ys

{-# COMPILE JS zip-with = _ => _ => _ => _ => _ => _ => f => as => bs => {
  let res = [];
  for (let i = 0; i < Math.min(as.length, bs.length); i++)
    res.push(f(as[i])(bs[i]));
  return res;
} #-}

infix 5 _to_

{-# TERMINATING #-}
_to_ : ℕ → ℕ → List ℕ
n to k = if k ≤ n then [] else n ∷ (suc n to k)
{-# COMPILE JS _to_ = n => k => Array(Math.max(0, Number(k - n))).fill(null).map((_, i) => BigInt(i) + n) #-}

any : (A → 𝔹) → List A → 𝔹
any p = foldr false λ b a → b ∨ p a

insert-after : ℕ → A → List A → List A
insert-after i e l = slice l 0 i ++ [ e ] ++ slice l (i + 1) ∥ l ∥

update-at : ℕ → A → List A → List A
update-at i e l = slice l 0 (i - 1) ++ [ e ] ++ slice l (i + 1) ∥ l ∥

Maybe-to-List : Maybe A → List A
Maybe-to-List nothing = []
Maybe-to-List (just x) = [ x ]

map-Maybe : (A → Maybe B) → List A → List B
map-Maybe f = concat-map λ a → Maybe-to-List (f a)

record List⁺ (A : Set a) : Set (lsuc a) where
  constructor _|:_
  field
    head : A
    tail : List A
open List⁺ public

module _ where
  open Semigroup {{ ... }}

  mconcat⁺ : {{ s : Semigroup A }} → List⁺ A → A
  mconcat⁺ (head |: tail) = foldr head _<>_ tail

module _ where
  open Monoid {{ ... }}

  mconcat : {{ m : Monoid A }} → List A → A
  mconcat {{ m }} as = foldr empty _<>_ as

instance
  List-Semigroup : Semigroup (List A)
  List-Semigroup = record { _<>_ = _++_ }

  List-Monoid : Monoid (List A)
  List-Monoid = record { semigroup = List-Semigroup ; empty = [] }

open import Effect.Applicative

module TraversableA (applicative : Applicative F) where
  open Applicative applicative

  sequenceA : List (F A) → F (List A)
  sequenceA = foldr (pure []) λ ac x → (_∷_ <$> x) <*> ac

  mapA : (A → F B) → List A → F (List B)
  mapA = sequenceA ∘₂ map

open import Effect.Monad

module TraversableM (monad : Monad M) where
  open Monad monad

  mapM : (A → M B) → List A → M (List B)
  mapM f = foldr (pure []) λ ac a → f a >>= λ b → (b ∷_) <$> ac

  forM : List A → (A → M B) → M (List B)
  forM = flip mapM

  -- TODO: Make this stack-safe
  foldM : ⦃ m : Monad M ⦄ → B → (A → B → M B) → List A → M B
  foldM b f [] = pure b
  foldM b f (x ∷ xs) = f x b >>= λ b' → foldM b' f xs

private module Tests where
  open import Agda.Builtin.Equality

  the : (A : Set) → A → A
  the A a = a

  _ : map (λ x → x + 1) [] ≡ []
  _ = refl

  _ : map (λ x → x + 1) (1 ∷ 2 ∷ 3 ∷ 4 ∷ []) ≡ (2 ∷ 3 ∷ 4 ∷ 5 ∷ [])
  _ = refl

  _ : ∀ {A} → the (List A) [] ++ [] ≡ []
  _ = refl

  _ : (1 ∷ 2 ∷ []) ++ (3 ∷ 4 ∷ []) ≡ 1 ∷ 2 ∷ 3 ∷ 4 ∷ []
  _ = refl

  _ : concat ([] ∷ (1 ∷ 2 ∷ 3 ∷ []) ∷ [] ∷ []) ≡ 1 ∷ 2 ∷ 3 ∷ []
  _ = refl

  _ : concat ((1 ∷ 2 ∷ []) ∷ (3 ∷ 4 ∷ []) ∷ []) ≡ 1 ∷ 2 ∷ 3 ∷ 4 ∷ []
  _ = refl

  _ : ∀ {A} → unsnoc (the (List A) []) ≡ nothing
  _ = refl

  _ : unsnoc (1 ∷ []) ≡ just ([] , 1)
  _ = refl

  _ : unsnoc (1 ∷ 2 ∷ 3 ∷ []) ≡ just (1 ∷ 2 ∷ [] , 3)
  _ = refl
