module Vscode.Window where

open import Agda.Builtin.Unit
open import Data.Maybe
open import Function

open import Data.IO

open import Vscode.Common
open import Vscode.SemanticTokensProvider

module TextEditor where
    postulate t : Set

    postulate active-editor : IO (Maybe t)
    {-# COMPILE JS active-editor = ({vscode}, cont) => {
      const e = vscode.window.activeTextEditor;
      cont(e ? (a => a["just"](e)) : (a => a["nothing"]()));
    } #-}

    postulate document : t → TextDocument.t
    {-# COMPILE JS document = editor => editor.document #-}

postulate on-did-change-active-text-editor-listener : (Maybe TextEditor.t → IO ⊤) → IO Disposable
{-# COMPILE JS on-did-change-active-text-editor-listener = update => ({vscode}, cont) =>
    cont(vscode.window.onDidChangeActiveTextEditor(editor => update(editor ? (a => a["just"](editor)) : (a => a["nothing"]()))(() => {}))) #-}
