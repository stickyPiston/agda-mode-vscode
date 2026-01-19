module Vscode.Panel where

open import TEA.System
open import TEA.Cmd as Cmd
open import TEA.Capability
open import Iepje.Internal.JS.Language.IO
open import Agda.Builtin.Unit
open import Prelude.List hiding (_++_)
open import Prelude.JSON
open import Iepje.Internal.Utils using (_>>_)
open import Prelude.String
open import Iepje.Internal.Utils using (case_of_)
open import Prelude.Maybe hiding (_>>=_ ; pure)
open import Prelude.Sigma

postulate text-document uri Panel : Set
postulate createWebviewPanel : vscode-api → IO Panel
{-# COMPILE JS createWebviewPanel = vscode => cont => cont(vscode.window.createWebviewPanel("window", "window", vscode.ViewColumn.One, { enableScripts: true }))  #-}

postulate setHtml : String → Panel → IO ⊤
{-# COMPILE JS setHtml = html => panel => cont => { panel.webview.html = html; cont(a => a["tt"]()) } #-}

postulate extensionUri : extension-context → String
{-# COMPILE JS extensionUri = context => context.extensionUri #-}

postulate joinPath : vscode-api → List String → String
{-# COMPILE JS joinPath = vscode => parts => vscode.Uri.joinPath(...parts) #-}

postulate toWebviewUri : Panel → String → String
{-# COMPILE JS toWebviewUri = panel => url => panel.webview.asWebviewUri(url) #-}

-- This should prolly be IO
postulate current-text-documents : vscode-api → List text-document
{-# COMPILE JS current-text-documents = vscode => vscode.workspace.textDocuments #-}

postulate get-document-uri : text-document → uri
{-# COMPILE JS get-document-uri = doc => doc.uri #-}

postulate uri-path uri-scheme : uri → String
{-# COMPILE JS uri-path = uri => uri.path #-}
{-# COMPILE JS uri-scheme = uri => uri.scheme #-}
    
postulate postMessage : Panel → JSON → IO ⊤
{-# COMPILE JS postMessage = panel => json => cont => { panel.webview.postMessage(json); cont(null) } #-}

postulate onMessage : Panel → extension-context → (JSON → IO ⊤) → IO ⊤
{-# COMPILE JS onMessage = panel => ctx => action => cont => { panel.webview.onDidReceiveMessage(msg => action(msg)(() => {}), undefined, ctx.subscriptions); cont(a => a["tt"]()); } #-}
    
sendMessage : ∀ {msg} {A} ⦃ c : Cloneable A ⦄ → Panel → A → Cmd msg
sendMessage panel m = Cmd.new λ _ → postMessage panel (encode m)

open-panel-cmd : ∀ {msg webview-msg} ⦃ c : Cloneable webview-msg ⦄ → System → (Panel → msg) → (webview-msg → msg) → Cmd msg
open-panel-cmd record { vscode = vscode ; context = context } panel-msg webview-msg = Cmd.new λ dispatch → do
    panel ← createWebviewPanel vscode
    dispatch (panel-msg panel)
    setHtml ("<html><body><main></main><script type=\"module\" src="
        ++ toWebviewUri panel (joinPath vscode (extensionUri context ∷ "out" ∷ "jAgda.AgdaMode.Webview.mjs" ∷ []))
        ++ "></script></body></html>") panel
    onMessage panel context λ json → case (decode json) of λ where
        (just wmsg) → dispatch (webview-msg wmsg)
        nothing     → pure tt
    pure tt

panel : ∀ {msg} → (panel-msg-type : Set) → ⦃ c : Cloneable panel-msg-type ⦄ → String → Capability msg
panel {msg} panel-msg-type html = record
  { requirement-type = ⊤
  ; new-requirement = λ _ → pure tt
  ; provided-type = just ((panel-msg-type → Cmd msg) , λ _ panel-msg → new λ dispatch → {!   !})
  ; register = {!   !}
  }
