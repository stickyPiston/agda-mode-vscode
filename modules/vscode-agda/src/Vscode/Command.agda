module Vscode.Command where

open import Agda.Builtin.Unit

open import Data.String
open import Data.IO

open import Vscode.Common

postulate register-command : String → IO ⊤ → IO Disposable
{-# COMPILE JS register-command = name => action => async () =>
  AgdaModeImports.vscode.commands.registerCommand(name, () => { action() }) #-}

postulate register-command-with-args : {A : Set} → String → (A → IO ⊤) → IO Disposable
{-# COMPILE JS register-command-with-args = _ => name => action => async () =>
  AgdaModeImports.vscode.commands.registerCommand(name, args => { action(args)() }) #-}

postulate execute-command : {A : Set} → String → A → IO ⊤
{-# COMPILE JS execute-command = _ => name => a => async () =>
  AgdaModeImports.vscode.commands.executeCommand(name, a) #-}
