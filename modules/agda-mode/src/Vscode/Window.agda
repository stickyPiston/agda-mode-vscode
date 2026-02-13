module Vscode.Window where

open import Agda.Builtin.Unit
open import Agda.Builtin.Nat
open import Data.String
open import Data.Maybe
open import Function

open import Data.IO

open import Vscode.Common
open import Vscode.SemanticTokensProvider

module TextEditor where
    postulate t : Set

    postulate active-editor : IO (Maybe t)
    {-# COMPILE JS active-editor = cont => {
      const e = AgdaModeImports.vscode.window.activeTextEditor;
      cont(e ? (a => a["just"](e)) : (a => a["nothing"]()));
    } #-}

    postulate document : t → TextDocument.t
    {-# COMPILE JS document = editor => editor.document #-}

postulate on-did-change-active-text-editor-listener : (Maybe TextEditor.t → IO ⊤) → IO Disposable
{-# COMPILE JS on-did-change-active-text-editor-listener = update => cont =>
    cont(AgdaModeImports.vscode.window.onDidChangeActiveTextEditor(editor =>
      update(editor ? (a => a["just"](editor)) : (a => a["nothing"]()))(() => {}))) #-}

module StatusBarItem where
  postulate t : Set

  data Alignment : Set where
    left right : Alignment

  encode-alignment : Alignment → Nat
  encode-alignment left = 1
  encode-alignment right = 2

  private module Internal where
    postulate create : String → Nat → Maybe Nat → IO t
    {-# COMPILE JS create = id => align => prio => cont => {
      const prio_ = prio({ "just": a => a, "nothing": () => undefined });
      cont(AgdaModeImports.vscode.window.createStatusBarItem(id, Number(align), prio_));
    } #-}

  -- TODO: StatusBarItems are disposable
  create : String → Alignment → Maybe Nat → IO t
  create id align priority = Internal.create id (encode-alignment align) priority

  postulate set-text : t → String → IO ⊤
  {-# COMPILE JS set-text = item => text => cont => {item.text = text; cont(a => a["tt"]());} #-}

  postulate show : t → IO ⊤
  {-# COMPILE JS show = item => cont => { item.show(); cont(a => a["tt"]()) } #-}
