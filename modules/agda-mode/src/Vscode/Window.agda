module Vscode.Window where

open import TEA.Capability
open import TEA.System
open import Agda.Builtin.Unit
open import Prelude.Maybe using (Maybe ; nothing ; just)
open import Prelude.Function
open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.Utils using (_>>_ ; _<$>_)
open import Vscode.SemanticTokensProvider

private postulate trace : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} → A → B → B
{-# COMPILE JS trace = _ => _ => _ => _ => thing => val => { console.log(thing) ; return val } #-}

module TextEditor where
    postulate t : Set

    postulate document : t → TextDocument.t
    {-# COMPILE JS document = editor => editor.document #-}

postulate on-did-change-active-text-editor-listener : vscode-api → (Maybe TextEditor.t → IO ⊤) → IO Disposable
{-# COMPILE JS on-did-change-active-text-editor-listener = vscode => update => cont =>
    cont(vscode.window.onDidChangeActiveTextEditor(editor => update(editor ? (a => a["just"](editor)) : (a => a["nothing"]()))(() => {}))) #-}

on-did-change-active-text-editor : ∀ {msg} → (Maybe TextEditor.t → msg) → Capability msg
on-did-change-active-text-editor text-editor-msg = record
  { requirement-type = ⊤
  ; new-requirement = λ x → pure tt
  ; provided-type = nothing
  ; register = λ sys _ update → just <$> on-did-change-active-text-editor-listener (sys .vscode) (update ∘ text-editor-msg)
  }