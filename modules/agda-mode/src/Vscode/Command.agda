module Vscode.Command where

open import Agda.Builtin.Unit

open import Data.String
open import Data.IO

open import Vscode.Common

postulate register-command : String → IO ⊤ → IO Disposable
{-# COMPILE JS register-command = name => action => (imports, cont) =>
  cont(imports.vscode.commands.registerCommand(name, () => { action(imports, _ => {}) })) #-}
