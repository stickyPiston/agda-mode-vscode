module Vscode.Panel where

open import Agda.Builtin.Unit

open import Data.IO as IO

open import Data.String
open import Data.Maybe
open import Agda.Builtin.Bool
open import Data.Int
open import Data.List
open import Agda.Builtin.Float
open import Data.JSON hiding (encode)
open import Data.Map

open import Vscode.Common

module ViewColumn where
  data t : Set where
    beside active one two three four five six seven eight nine : t

  encode : t → Int
  encode beside = - + 2
  encode active = - + 1
  encode one = + 1
  encode two = + 2
  encode three = + 3
  encode four = + 4
  encode five = + 5
  encode six = + 6
  encode seven = + 7
  encode eight = + 8
  encode nine = + 9

module WebviewOptions where
  record t : Set where field
    enable-command-uris : List String
    enable-forms enable-scripts : Bool
    -- local-resource-roots : List Uri.t
    -- port-mapping : List WebviewPortMapping.t
  open t public

  default : t
  default = record
    { enable-command-uris = []
    ; enable-forms = false
    ; enable-scripts = false
    }

  encode : t → JSON
  encode t = j-object
    (  ("enableCommandUris" ↦ j-array (map j-string (t .enable-command-uris)))
    <> ("enableForms" ↦ j-bool (t .enable-forms))
    <> ("enableScripts" ↦ j-bool (t .enable-scripts))
    )

module ShowOptions where
  record t : Set where field
    preserve-focus : Bool
    view-column : ViewColumn.t
  open t public

  postulate fromℤ : Int → Float
  {-# COMPILE JS fromℤ = Number #-}

  encode : t → JSON
  encode t = j-object
    (  ("preserveFocus" ↦ j-bool (t .preserve-focus))
    <> ("viewColumn" ↦ j-number (fromℤ (ViewColumn.encode (t .view-column))))
    )

module Panel where
  open import Function
  open import Data.JSON
  open import Level

  private variable ℓ : Level

  postulate t : Set → Set

  module Internal where
    postulate create : ∀ {A} → String → String → JSON → JSON → IO (t A)
    {-# COMPILE JS create = _ => viewType => title => showOptions => options => cont =>
      cont(AgdaModeImports.vscode.window.createWebviewPanel(viewType, title, showOptions, options)) #-}

    postulate post-message : ∀ {A} → t A → JSON → IO ⊤
    {-# COMPILE JS post-message = panel => json => cont => { panel.webview.postMessage(json); cont(a => a["tt"]()) } #-}

    postulate on-message : ∀ {A} → t A → (JSON → IO ⊤) → IO Disposable
    {-# COMPILE JS on-message = _ => panel => listener => cont =>
      cont(panel.webview.onDidReceiveMessage(msg => listener(msg)(_ => {}))) #-}

  postulate set-html : ∀ {A} → t A → String → IO ⊤
  {-# COMPILE JS set-html = _ => panel => html => cont => { panel.webview.html = html; cont(a => a["tt"]()) } #-}

  postulate to-webview-uri : ∀ {A} → t A → Uri.t → Uri.t
  {-# COMPILE JS to-webview-uri = _ => panel => url => panel.webview.asWebviewUri(url) #-}

  create : ∀ {A} ⦃ c : Cloneable A ⦄ → String → String → ShowOptions.t → WebviewOptions.t → IO (t A)
  create view-type title show-options options =
    Internal.create view-type title (ShowOptions.encode show-options) (WebviewOptions.encode options)

  send-message : ∀ {A} ⦃ c : Cloneable A ⦄ → t A → A → IO ⊤
  send-message panel a = Internal.post-message panel (encode a)

  open import Effect.Monad
  open IO.Effectful
  open Monad ⦃ ... ⦄

  on-message : ∀ {A} ⦃ c : Cloneable A ⦄ → t A → (A → IO ⊤) → IO Disposable
  on-message panel listener = Internal.on-message panel λ json → case decode json of λ where
    nothing → pure tt
    (just a) → listener a
