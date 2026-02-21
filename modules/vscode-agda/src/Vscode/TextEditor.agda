module Vscode.TextEditor where

open import Data.String
open import Data.IO
open import Data.List
open import Data.Bool
open import Data.Maybe
open import Function

open import Agda.Builtin.Nat
open import Agda.Builtin.Unit

open import Vscode.Common

module DecorationType where
  module Options where
    postulate t : Set

    postulate set-colour : String → t → t
    {-# COMPILE JS set-colour = c => t => ({ ...t, color: c }) #-}
    postulate set-text-decoration : String → t → t
    {-# COMPILE JS set-text-decoration = d => t => ({ ...t, textDecoration: d }) #-}

    postulate new : IO t
    {-# COMPILE JS new = async () => ({}) #-}

  postulate t : Set

  postulate create : Options.t → IO t
  {-# COMPILE JS create = options => async () =>
    AgdaModeImports.vscode.window.createTextEditorDecorationType(options) #-}

module Edit where
  postulate EditBuilder : Set

  t : Set
  t = EditBuilder → IO ⊤

  postulate delete : Range.t → EditBuilder → IO ⊤
  {-# COMPILE JS delete = r => b => async () => {b.delete(r); return a => a["tt"]()} #-}

  postulate insert : Position.t → String → EditBuilder → IO ⊤
  {-# COMPILE JS insert = p => s => b => async () => {b.insert(p, s); return a => a["tt"]()} #-}

  postulate replace : Range.t → String → EditBuilder → IO ⊤
  {-# COMPILE JS replace = r => s => b => async () => {b.replace(r, s); return a => a["tt"]()} #-}

module TextEditor where
  postulate t : Set

  postulate active-editor : IO (Maybe t)
  {-# COMPILE JS active-editor = async () => {
    const e = AgdaModeImports.vscode.window.activeTextEditor;
    return e ? (a => a["just"](e)) : (a => a["nothing"]());
  } #-}

  postulate document : t → TextDocument.t
  {-# COMPILE JS document = editor => editor.document #-}

  postulate cursor-pos : t → IO Position.t
  {-# COMPILE JS cursor-pos = ed => async () => ed.selection.active #-}

  postulate set-decoration : DecorationType.t → Range.t → t → IO ⊤
  {-# COMPILE JS set-decoration = d => r => t => async () => {
    t.setDecorations(d, [r]);
    return a => a["tt"]();
  } #-}

  postulate remove-decoration : DecorationType.t → t → IO ⊤
  {-# COMPILE JS remove-decoration = d => t => async () => {
    t.setDecorations(d, []);
    return a => a["tt"]();
  } #-}

  module Internal where
    postulate edit : (Edit.EditBuilder → IO ⊤) → t → IO 𝔹
    {-# COMPILE JS edit = f => e => () => e.edit(b => f(b)()) #-}

  open import Effect.Functor
  open Functor ⦃ ... ⦄

  edit : List Edit.t → t → IO 𝔹
  edit edits = Internal.edit λ b →
    let open TraversableM Effectful.monad
     in tt <$ forM edits (_$ b)
