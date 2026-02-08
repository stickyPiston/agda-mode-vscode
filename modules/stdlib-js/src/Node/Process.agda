module Node.Process where

open import Data.String
open import Data.List
open import Data.IO
open import Agda.Builtin.Unit

module Buffer where
    postulate t : Set

    postulate from : String → t
    {-# COMPILE JS from = Buffer.from #-}

    postulate to-string : t → String
    {-# COMPILE JS to-string = buf => buf.toString() #-}

module Process where
    postulate t : Set

    postulate spawn : String → List String → IO t
    {-# COMPILE JS spawn = cmd => args => cont => {
      const p = AgdaModeImports.process.spawn(cmd, args) ; cont(p)
    } #-}

    postulate write : t → String → IO ⊤
    {-# COMPILE JS write = proc => chunk => cont => {
      proc.stdin.write(chunk);
      cont(a => a["tt"]())
    } #-}

    postulate read : t → IO String
    {-# COMPILE JS read = proc => cont => cont(proc.stdout.read()) #-}

    -- TODO: Return Disposable
    postulate on-data : t → (Buffer.t → IO ⊤) → IO ⊤
    {-# COMPILE JS on-data = proc => handler => cont => {
        proc.stdout.on("data", data => { handler(data)(() => {}) });
        cont(a => a["tt"]())
    } #-}
