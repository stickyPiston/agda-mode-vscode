module Vscode.Command where

open import Iepje.Internal.JS.Language.IO
open import Prelude.Maybe hiding (_<$>_ ; _>>=_ ; pure)
open import Iepje.Internal.Utils using (_<$>_)
open import Prelude.String
open import Agda.Builtin.Unit
open import TEA.Cmd
open import TEA.System
open System
open import TEA.Capability
open Capability

postulate register-command : vscode-api → String → IO ⊤ → IO Disposable
{-# COMPILE JS register-command = vscode => name => action => cont => { cont(vscode.commands.registerCommand(name, () => { action(_ => {}) })) } #-}

command : ∀ {msg} → String → msg → Capability msg
command name m = record
    { requirement-type = ⊤
    ; new-requirement = λ _ → pure tt
    ; provided-type = nothing
    ; register = λ system tt update → just <$> register-command (system .vscode) name (update m)
    }