module TEA.System where

postulate process-api vscode-api extension-context : Set

record System : Set where field
    process : process-api
    vscode  : vscode-api
    context : extension-context
