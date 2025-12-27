module Vscode.StdioProcess where

open import TEA.Capability
open import TEA.System
open System
open import TEA.Cmd as Cmd

open import Prelude.String
open import Prelude.List
open import Prelude.Maybe
open import Prelude.Sigma
open import Agda.Builtin.Unit

open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.Utils using (_>>_)

module Buffer where
    postulate t : Set

    postulate from : String → t
    {-# COMPILE JS from = Buffer.from #-}

    postulate toString : t → String
    {-# COMPILE JS toString = buf => buf.toString() #-}

module Process where
    postulate t : Set

    postulate spawn : process-api → String → List String → IO t
    {-# COMPILE JS spawn = process => cmd => args => cont => { const p = process.spawn(cmd, args) ; cont(p) } #-}

    postulate write : t → String → IO ⊤
    {-# COMPILE JS write = process => chunk => cont => { process.stdin.write(chunk); process.once("drain", () => {}); cont(a => a["tt"]()) } #-}

    postulate read : t → IO String
    {-# COMPILE JS read = proc => cont => cont(proc.stdout.read()) #-}

    postulate on-data : t → (Buffer.t → IO ⊤) → IO ⊤
    {-# COMPILE JS on-data = proc => handler => cont => {
        proc.stdout.on("data", data => { handler(data)(() => {}) });
        cont(a => a["tt"]())
    } #-}

stdio-process : ∀ {msg} → (name : String) → (args : List String) → (Buffer.t → msg) → Capability msg
stdio-process {msg} name args on-data-msg = record
    { requirement-type = Process.t
    ; new-requirement = λ sys → Process.spawn (sys .process) name args
    ; provided-type = just ((String → Cmd msg) , λ proc input → Cmd.new λ _ → Process.write proc input)
    ; register = λ sys proc update → do
        Process.on-data proc λ buf → update (on-data-msg buf)
        pure nothing -- TODO: Figure out if processes can be disposable in vscode extension contexts
    }