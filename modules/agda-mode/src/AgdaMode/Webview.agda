module AgdaMode.Webview where

open import Iepje.Prelude hiding (Maybe ; nothing ; just)

main : IO ⊤
main = display "main" (text "Hello world!")
