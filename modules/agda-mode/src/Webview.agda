module Webview where

open import Communication

open import Iepje.Prelude
open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.PrimitiveTypes
open import Iepje.Internal.JS.WebAPIs.DOM
open import Iepje.Internal.Effect

postulate onMessage : Window → (null → IO ⊤) → IO null
{-# COMPILE JS onMessage = win => dispatch => cont => { win.addEventListener("message", () => { console.log(dispatch); dispatch(null)(() => {}) }); cont(null) } #-}

postulate vsc-api : Set

data Cmd : Set where
    message-received button-pressed : Cmd
    acquired-vscode : vsc-api → Cmd

message-effect : Effect Cmd
message-effect = from λ dispatch → do
  w ← document >>= get-defaultView
  _ ← onMessage w λ _ → dispatch message-received
  pure tt

postulate acquire-vscode : vsc-api
{-# COMPILE JS acquire-vscode = acquireVsCodeApi() #-}

acquire-vscode-effect : Effect Cmd
acquire-vscode-effect = from λ dispatch → dispatch $ acquired-vscode acquire-vscode

postulate internal-post-message : vsc-api → JSON → IO null
{-# COMPILE JS internal-post-message = api => msg => cont => { api.postMessage(msg); cont(null) } #-}

postulate log : ∀ { A : Set } → A → IO null
{-# COMPILE JS log = _ => thing => cont => { console.log(thing); cont(null) } #-}

post-message : ∀ { A msg : Set } ⦃ r : Cloneable msg ⦄ → vsc-api → msg → Effect A
post-message vsc msg = from λ _ → do
    _ ← internal-post-message vsc (encode msg)
    pure tt

main : IO ⊤
main = interactIO "main"
  ((0 , nothing) , batch (message-effect ∷ acquire-vscode-effect ∷ []))
  (λ n → pure do
    text $ primShowNat (fst n) ++ " messages received"
    button button-pressed $ text "send message")
  (λ cmd model → case cmd of λ where
    message-received → pure ((1 + fst model , snd model) , none)
    button-pressed → case snd model of λ where
      (just vsc) → pure (model , post-message vsc a)
      nothing → pure (model , none)
    (acquired-vscode vsc) → pure ((fst model , just vsc) , none))