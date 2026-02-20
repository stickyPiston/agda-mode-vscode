module Vscode.Common where

open import Data.List
open import Data.String
open import Agda.Builtin.Nat
open import Agda.Builtin.Unit

open import Data.IO

postulate Disposable : Set

module Uri where
  postulate t : Set

  postulate path scheme : t → String
  {-# COMPILE JS path = uri => uri.path #-}
  {-# COMPILE JS scheme = uri => uri.scheme #-}

  postulate file : String → t
  {-# COMPILE JS file = path => AgdaModeImports.vscode.URI.file(path) #-}

module Position where
    postulate t : Set
    -- TODO: Turn t into a record
    postulate new : Nat → Nat → t
    postulate line char : t → Nat

    {-# COMPILE JS new = line => char => new AgdaModeImports.vscode.Position(Number(line), Number(char)) #-}
    {-# COMPILE JS line = pos => BigInt(pos.line) #-}
    {-# COMPILE JS char = pos => BigInt(pos.character) #-}

module Range where
  open import Data.Bool

  postulate t : Set
  -- Technically not pure, because === will still call two new objects different,
  -- but for all intents and purposes it does act purely.
  postulate new : Position.t → Position.t → t
  postulate start end : t → Position.t

  {-# COMPILE JS new = start => end => new AgdaModeImports.vscode.Range(start, end) #-}
  {-# COMPILE JS start = range => range.start #-}
  {-# COMPILE JS end = range => range.end #-}

  postulate _in-range_ : Position.t → t → 𝔹
  {-# COMPILE JS _in-range_ = pos => range => range.contains(pos) #-}

module TextLine where
    postulate t : Set

    postulate range : t → Range.t
    {-# COMPILE JS range = line => line.range #-}

module TextDocument where
    postulate t : Set

    postulate uri : t → Uri.t
    {-# COMPILE JS uri = doc => doc.uri #-}

    postulate get-text : t → String
    {-# COMPILE JS get-text = doc => doc.getText() #-}

    postulate position-at : t → Nat → Position.t
    {-# COMPILE JS position-at = doc => n => doc.positionAt(Number(n)) #-}

    postulate line-at : t → Nat → TextLine.t
    {-# COMPILE JS line-at = doc => n => doc.lineAt(Number(n)) #-}

    postulate file-name : t → String
    {-# COMPILE JS file-name = doc => doc.fileName #-}

    postulate open-path : String → IO t
    {-# COMPILE JS open-path = path => () => AgdaModeImports.vscode.workspace.openTextDocument(path) #-}

module ExtensionContext where
  postulate t : Set

  postulate get : IO t
  {-# COMPILE JS get = async () => context #-}

  postulate extension-uri : t → Uri.t
  {-# COMPILE JS extension-uri = ctx => ctx.extensionUri #-}

module DocumentSelector where
  open import Data.JSON hiding (encode)
  open import Data.Map

  data t : Set where
    language scheme path-pattern : String → t
    _∩_ : t → t → t

  encode : t → JSON
  encode filter = j-object (kvs filter)
    where
      kvs : t → StringMap.t JSON
      kvs (language x) = "language" ↦ j-string x
      kvs (scheme x) = "scheme" ↦ j-string x
      kvs (path-pattern x) = "pattern" ↦ j-string x
      kvs (l ∩ r) = kvs l <> kvs r
open DocumentSelector using (language ; scheme ; path-pattern ; _∩_) public

module CancellationToken where
  postulate t : Set

module Location where
  record t : Set where
    constructor new
    field
      uri : Uri.t
      pos : Position.t

  {-# COMPILE JS t = ((loc, v) => v["new"](loc.uri, loc.range)) #-}
  {-# COMPILE JS new = uri => pos => new AgdaModeImports.vscode.Location(uri, pos) #-}
  {-# COMPILE JS t.uri = loc => loc.uri #-}
  {-# COMPILE JS t.pos = loc => loc.range #-}

module DefinitionProvider where
  open import Data.Maybe

  postulate t : Set

  postulate new : (TextDocument.t → Position.t → CancellationToken.t → IO (Maybe Location.t)) → IO t
  {-# COMPILE JS new = f => async () => ({ provideDefinition: async (d, p, c) => {
    const ml = await f(d)(p)(c)();
    const m = ml({ "nothing": () => undefined, "just": (loc) => loc });
    return m;
  } }) #-}

  private module Internal where
    open import Data.JSON

    postulate register : JSON → t → IO Disposable
    {-# COMPILE JS register = selector => provider => async () =>
      AgdaModeImports.vscode.languages.registerDefinitionProvider(selector, provider) #-}

  register : DocumentSelector.t → t → IO Disposable
  register selector t = Internal.register (DocumentSelector.encode selector) t
