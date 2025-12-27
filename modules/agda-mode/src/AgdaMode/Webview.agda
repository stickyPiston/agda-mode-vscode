module AgdaMode.Webview where

open import AgdaMode.Common.Communication
open import Prelude.JSON

open import Iepje.Prelude hiding (Maybe ; nothing ; just)
open import Prelude.Maybe
open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.PrimitiveTypes
open import Iepje.Internal.JS.WebAPIs.DOM
open import Iepje.Internal.Effect

postulate onMessage : Window → (JSON → IO ⊤) → IO null
{-# COMPILE JS onMessage = win => dispatch => cont => { win.addEventListener("message", msg => { console.log(msg.data); dispatch(msg.data)(() => {}) }); cont(null) } #-}

postulate vsc-api : Set

data Cmd : Set where
    message-received : Maybe WebviewMsg → Cmd
    acquired-vscode : vsc-api → Cmd

message-effect : Effect Cmd
message-effect = from λ dispatch → do
  w ← document >>= get-defaultView
  _ ← onMessage w λ m → dispatch $ message-received (decode m)
  pure tt

postulate acquire-vscode : vsc-api
{-# COMPILE JS acquire-vscode = acquireVsCodeApi() #-}

acquire-vscode-effect : Effect Cmd
acquire-vscode-effect = from λ dispatch → dispatch $ acquired-vscode acquire-vscode

postulate internal-post-message : vsc-api → JSON → IO null
{-# COMPILE JS internal-post-message = api => msg => cont => { api.postMessage(msg); cont(null) } #-}

post-message : { A msg : Set } ⦃ r : Cloneable msg ⦄ → vsc-api → msg → Effect A
post-message vsc msg = from λ _ → do
    _ ← internal-post-message vsc (encode msg)
    pure tt

main : IO ⊤
main = interactIO "main"
  ((0 , nothing) , batch (message-effect ∷ acquire-vscode-effect ∷ []))
  (λ (n , _) → pure $ text $ primShowNat n ++ " errors")
  (λ cmd model → case cmd of λ where
    (message-received nothing) → pure (model , none)
    (message-received (just (show-errors x))) → pure ((x , (snd model)) , none)
    (acquired-vscode vsc) → pure ((fst model , just vsc) , none))