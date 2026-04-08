module Vscode.TextEditor where

open import Data.String
open import Data.IO
open import Data.List
open import Data.Bool
open import Data.Maybe
open import Data.Nat
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

    postulate set-background-colour : String → t → t
    {-# COMPILE JS set-background-colour = c => t => ({ ...t, backgroundColor: c }) #-}

    postulate new : IO t
    {-# COMPILE JS new = async () => ({}) #-}

  postulate t : Set

  postulate create : Options.t → IO t
  {-# COMPILE JS create = options => async () =>
    AgdaModeImports.vscode.window.createTextEditorDecorationType(options) #-}

module Edit where
  postulate EditBuilder : Set

  t : Set
  t = EditBuilder → EditBuilder

  postulate delete : Range.t → t
  {-# COMPILE JS delete = r => b => { b.delete(r); return b } #-}

  postulate insert : Position.t → String → t
  {-# COMPILE JS insert = p => s => b => { b.insert(p, s); return b } #-}

  postulate replace : Range.t → String → t
  {-# COMPILE JS replace = r => s => b => { b.replace(r, s); return b } #-}

module TextEditor where
  postulate t : Set

  postulate active-editor : IO (Maybe t)
  {-# COMPILE JS active-editor = async () => {
    const e = AgdaModeImports.vscode.window.activeTextEditor;
    return e ? (a => a["just"](e)) : (a => a["nothing"]());
  } #-}

  postulate document : t → IO TextDocument.t
  {-# COMPILE JS document = editor => async () => editor.document #-}

  postulate cursor-pos : t → IO Position.t
  {-# COMPILE JS cursor-pos = ed => async () => ed.selection.active #-}

  postulate set-decoration : DecorationType.t → List Range.t → t → IO ⊤
  {-# COMPILE JS set-decoration = d => r => t => async () => {
    t.setDecorations(d, r);
    return a => a["tt"]();
  } #-}

  postulate remove-decoration : DecorationType.t → t → IO ⊤
  {-# COMPILE JS remove-decoration = d => t => async () => {
    t.setDecorations(d, []);
    return a => a["tt"]();
  } #-}

  module Internal where
    postulate edit : Edit.t → t → IO Bool
    {-# COMPILE JS edit = f => e => async () => { return await e.edit(b => f(b)) } #-}

  open import Effect.Functor
  open Functor ⦃ ... ⦄

  edit : List Edit.t → t → IO Bool
  edit edits = Internal.edit λ b → edits |> foldr b _|>_

module TextDocumentContentChangeEvent where
  record t : Set where
    constructor mkTextDocumentContentChangeEvent
    field
      range : Range.t
      text : String
      range-length range-offset : ℕ
  open t public

  {-# COMPILE JS t = (({ range, text, rangeLength, rangeOffset }, v) =>
    v["mkTextDocumentContentChangeEvent"](range, text, BigInt(rangeLength), BigInt(rangeOffset))) #-}
  {-# COMPILE JS mkTextDocumentContentChangeEvent = range => text => rangeLength => rangeOffset =>
    ({ range, text, rangeLength, rangeOffset }) #-}
  {-# COMPILE JS t.range = ({ range }) => range #-}
  {-# COMPILE JS t.text = ({ text }) => text #-}
  {-# COMPILE JS t.range-length = ({ rangeLength }) => BigInt(rangeLength) #-}
  {-# COMPILE JS t.range-offset = ({ rangeOffset }) => BigInt(rangeOffset) #-}
open TextDocumentContentChangeEvent using (range ; text ; range-length ; range-offset) public

module TextDocumentChangeReason where
  data t : Set where
    undo redo : t

  -- In the vscode API, this is encoded via an enum, we just encode their values
  -- directly
  {-# COMPILE JS t = ((x, v) => x === 1 ? v["undo"]() : v["redo"]()) #-}
  {-# COMPILE JS undo = 1 #-}
  {-# COMPILE JS redo = 2 #-}

module TextDocumentChangeEvent where
  record t : Set where
    constructor mkTextDocumentChangeEvent
    field
      content-changes : List TextDocumentContentChangeEvent.t
      document : TextDocument.t
      reason : Maybe TextDocumentChangeReason.t
  open t public

  -- We need to convert reason to the Agda representation of a Maybe, so that
  -- its pattern matching function gets the value it expects
  {-# COMPILE JS t = (({ contentChanges, document, reason }, v) =>
    v["mkTextDocumentChangeEvent"](contentChanges, document, reason ? (a => a["just"](reason)) : (a => a["nothing"]()))) #-}
  {-# COMPILE JS mkTextDocumentChangeEvent = contentChanges => document => reason =>
    ({ contentChanges, document, reason: reason({ "just": (r) => r, "nothing": () => undefined }) }) #-}
  {-# COMPILE JS t.content-changes = ({ contentChanges }) => contentChanges #-}
  {-# COMPILE JS t.document = ({ document }) => document #-}
  {-# COMPILE JS t.reason = ({ reason }) => reason ? (a => a["just"](reason)) : (a => a["nothing"]()) #-}
open TextDocumentChangeEvent using (content-changes ; document ; reason ; mkTextDocumentChangeEvent) public

module Workspace where
  -- TODO: Handle disposable
  postulate on-did-change-text-document : (TextDocumentChangeEvent.t → IO ⊤) → IO ⊤
  {-# COMPILE JS on-did-change-text-document = handler => async () => {
    AgdaModeImports.vscode.workspace.onDidChangeTextDocument(e => handler(e)());
    return a => a["tt"]();
  } #-}
