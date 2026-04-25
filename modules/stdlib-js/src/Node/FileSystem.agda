module Node.FileSystem where

open import Data.String hiding (join)
open import Data.IO
open import Data.List
open import Data.Maybe

postulate load-file : String → IO (Maybe String)
{-# COMPILE JS load-file = path => async () => {
  try {
    const content = await AgdaModeImports.fs.readFile(path);
    return a => a["just"](content);
  } catch (_e) {
    return a => a["nothing"]();
  }
} #-}

module Path where
  postulate join : List String → String
  {-# COMPILE JS join = segments => AgdaModeImports.path.join(...segments) #-}