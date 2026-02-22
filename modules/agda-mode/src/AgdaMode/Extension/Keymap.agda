module AgdaMode.Extension.Keymap where

open import Data.List
open import Data.JSON
open import Data.JSON.Decode
open import Data.String
open import Data.IO
open import Data.Maybe
open import Data.Maybe.Effectful
open import Data.Map
open import Data.Product
open import Function

open import Effect.Monad

open import Node.FileSystem

private variable A : Set

open Monad ⦃ ... ⦄
open MonadPlus ⦃ ... ⦄ using (_<|>_ ; ⊘)

module Trie where
  record t : Set where
    inductive
    constructor trie
    field
      values : List String
      subtrees : List (String × t)
  open t public

  empty : t
  empty = record { values = [] ; subtrees = [] }

open Trie using (trie ; values ; subtrees) public

{-# TERMINATING #-}
trie-decoder : Decoder Trie.t
trie-decoder =
  let values-decoder = from-Maybe [] <$> optional ">>" (list string)
      subtrees-decoder = λ where
        (j-object o) → just $ concat-for (StringMap.entries o) λ where
          (">>" , _) → []
          (k , v) → case trie-decoder v of λ where
            (just t) → [ k , t ]
            nothing → []
        _ → nothing
   in ⦇ trie values-decoder subtrees-decoder ⦈

load-keymap : String → IO (Maybe Trie.t)
load-keymap = fmap (parse-json >=> trie-decoder) ∘ load-file

-- Θ(min(|xs|, depth(t)))
match : List String → Trie.t → Maybe Trie.t
match [] t = just t
match (x ∷ xs) t = find (λ (k , _) → k == x) (t .subtrees) >>= match xs ∘ proj₂

-- Suggest which characters can be used to continue traverse the trie
next-characters : Trie.t → List String
next-characters = sort ∘ map proj₁ ∘ subtrees
