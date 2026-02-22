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

record Trie : Set where
  inductive
  constructor trie
  field
    values : List String
    subtrees : List (String × Trie)

{-# TERMINATING #-}
trie-decoder : Decoder Trie
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

load-keymap : String → IO (Maybe Trie)
load-keymap = fmap (parse-json >=> trie-decoder) ∘ load-file
