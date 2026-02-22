module Node.FileSystem where

open import Data.String
open import Data.IO

postulate load-file : String → IO String
{-# COMPILE JS load-file = path => () => AgdaModeImports.fs.readFile(path) #-}
