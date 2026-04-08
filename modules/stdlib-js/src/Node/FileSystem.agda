module Node.FileSystem where

open import Data.String hiding (join)
open import Data.IO
open import Data.List

postulate load-file : String → IO String
{-# COMPILE JS load-file = path => () => AgdaModeImports.fs.readFile(path) #-}

module Path where
  postulate join : List String → String
  {-# COMPILE JS join = segments => AgdaModeImports.path.join(...segments) #-}