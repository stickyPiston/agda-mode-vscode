module Vscode.Logging where

open import Data.String
open import Data.IO
open import Agda.Builtin.Unit

module OutputChannel where
  postulate t : Set

  postulate create : String → IO t
  {-# COMPILE JS create = name => async () =>
    AgdaModeImports.vscode.window.createOutputChannel(name, { log: true }) #-}

  postulate debug trace info warn error : String → t → IO ⊤
  {-# COMPILE JS debug = msg => chan => async () => chan.debug(msg) #-}
  {-# COMPILE JS trace = msg => chan => async () => chan.trace(msg) #-}
  {-# COMPILE JS info = msg => chan => async () => chan.info(msg) #-}
  {-# COMPILE JS warn = msg => chan => async () => chan.warn(msg) #-}
  {-# COMPILE JS error = msg => chan => async () => chan.error(msg) #-}