module Vscode.Window where

open import Agda.Builtin.Unit
open import Agda.Builtin.Nat
open import Data.String
open import Data.Maybe
open import Function

open import Data.IO
open import Data.JSON hiding (encode)

open import Vscode.Common
open import Vscode.SemanticTokensProvider

module TextEditor where
    postulate t : Set

    postulate active-editor : IO (Maybe t)
    {-# COMPILE JS active-editor = async () => {
      const e = AgdaModeImports.vscode.window.activeTextEditor;
      return e ? (a => a["just"](e)) : (a => a["nothing"]());
    } #-}

    postulate document : t → TextDocument.t
    {-# COMPILE JS document = editor => editor.document #-}

postulate on-did-change-active-text-editor-listener : (Maybe TextEditor.t → IO ⊤) → IO Disposable
{-# COMPILE JS on-did-change-active-text-editor-listener = update => async () =>
    AgdaModeImports.vscode.window.onDidChangeActiveTextEditor(editor =>
      update(editor ? (a => a["just"](editor)) : (a => a["nothing"]()))(() => {})) #-}

module StatusBarItem where
  postulate t : Set

  data Alignment : Set where
    left right : Alignment

  encode-alignment : Alignment → Nat
  encode-alignment left = 1
  encode-alignment right = 2

  private module Internal where
    postulate create : String → Nat → Maybe Nat → IO t
    {-# COMPILE JS create = id => align => prio => async () => {
      const prio_ = prio({ "just": a => a, "nothing": () => undefined });
      return AgdaModeImports.vscode.window.createStatusBarItem(id, Number(align), prio_);
    } #-}

  -- TODO: StatusBarItems are disposable
  create : String → Alignment → Maybe Nat → IO t
  create id align priority = Internal.create id (encode-alignment align) priority

  postulate set-text : t → String → IO ⊤
  {-# COMPILE JS set-text = item => text => async () => { item.text = text; return a => a["tt"]() } #-}

  postulate show : t → IO ⊤
  {-# COMPILE JS show = item => async () => { item.show(); return a => a["tt"]() } #-}

module TextDocumentShowOptions where
  open import Data.Bool
  open import Vscode.Panel

  record t : Set where field
    preserve-focus preview : 𝔹
    selection : Range.t
    view-column : ViewColumn.t
  open t public

module Window where
  open import Vscode.Panel
  open TextDocumentShowOptions

  module Internal where
    open import Data.Int

    postulate show-text-document : Uri.t → TextDocumentShowOptions.t → Int → IO TextEditor.t
    {-# COMPILE JS show-text-document = uri => options => viewColumn => () => new Promise((resolve, reject) =>
      options["record"]({
        record: (a, b, c, _) =>
          AgdaModeImports.vscode.window.showTextDocument(uri, {
            preserveFocus: a, preview: b, selection: c, viewColumn: viewColumn
          }).then(resolve).catch(reject)
      })
    ) #-}

  show-text-document : Uri.t → TextDocumentShowOptions.t → IO TextEditor.t
  show-text-document uri options =
    Internal.show-text-document uri options (ViewColumn.encode (options .view-column))
