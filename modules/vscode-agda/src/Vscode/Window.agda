module Vscode.Window where

open import Agda.Builtin.Unit
open import Agda.Builtin.Nat
open import Data.String hiding (show)
open import Data.Maybe
open import Data.Bool
open import Data.List
open import Function

open import Data.IO
open import Data.JSON hiding (encode)

open import Vscode.Common
open import Vscode.SemanticTokensProvider
open import Vscode.TextEditor

postulate on-did-change-active-text-editor-listener : (Maybe TextEditor.t → IO ⊤) → IO Disposable.t
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

  postulate hide : t → IO ⊤
  {-# COMPILE JS hide = item => async () => { item.hide(); return a => a["tt"]() } #-}

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

  postulate show-input-box : IO (Maybe String)
  {-# COMPILE JS show-input-box = async () => {
    const answer = await AgdaModeImports.vscode.window.showInputBox();
    return answer ? (a => a["just"](answer)) : (a => a["nothing"]());
  } #-}

  postulate quick-pick : List String → IO String
  {-# COMPILE JS quick-pick = options => async () => {
    return await AgdaModeImports.vscode.window.showQuickPick(options);
  } #-}

  postulate show-information-message show-error-message : {A : Set} → String → List A → IO (Maybe A)
  {-# COMPILE JS show-information-message = _ => msg => items => async () => {
    const answer = await AgdaModeImports.vscode.window.showInformationMessage(msg, items);
    return answer ? (a => a["just"](answer)) : (a => a["nothing"]());
  } #-}
  {-# COMPILE JS show-error-message = msg => items => async () => {
    const answer = await AgdaModeImports.vscode.window.showErrorMessage(msg, items);
    return answer ? (a => a["just"](answer)) : (a => a["nothing"]());
  } #-}
