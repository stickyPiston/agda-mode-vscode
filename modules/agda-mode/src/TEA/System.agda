module TEA.System where

postulate process-api vscode-api extension-context : Set

record System : Set where field
    process : process-api
    vscode  : vscode-api
    context : extension-context
open System public

{-# COMPILE JS System = ((x, v) => v["record"](x.process, x.vscode, x.context)) #-}
{-# COMPILE JS System.process = ({ process }) => process #-}
{-# COMPILE JS System.vscode = ({ vscode }) => vscode #-}
{-# COMPILE JS System.context = ({ context }) => context #-}