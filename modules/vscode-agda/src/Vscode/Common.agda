module Vscode.Common where

open import Data.List
open import Data.String
open import Data.Nat
open import Data.Product
open import Data.Maybe
open import Data.Bool
open import Agda.Builtin.Unit

open import Data.IO

module Disposable where
  postulate t : Set

  postulate new : IO ⊤ → t
  {-# COMPILE JS new = dispose => new AgdaModeImports.vscode.Disposable(dispose) #-}

  postulate dispose : t → IO ⊤
  {-# COMPILE JS dispose = disposable => disposable.dispose() #-}

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

    left : Nat → t → t
    left n t = new (line t) (char t - n)

    right : Nat → t → t
    right n t = new (line t) (char t + n)

module Range where
  open import Data.Bool
  open import Function

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

  postulate _∩_ : t → t → Maybe t
  {-# COMPILE JS _∩_ = a => b => {
    const i = a.intersection(b);
    return i ? (x => x["just"](i)) : (x => x["nothing"]());
  } #-}

module TextLine where
    postulate t : Set

    postulate range : t → Range.t
    {-# COMPILE JS range = line => line.range #-}

module TextDocument where
    postulate t : Set

    postulate uri : t → Uri.t
    {-# COMPILE JS uri = doc => doc.uri #-}

    postulate get-text : Range.t → t → String
    {-# COMPILE JS get-text = range => doc => doc.getText(range) #-}

    postulate get-all-text : t → IO String
    {-# COMPILE JS get-all-text = doc => async () => doc.getText() #-}

    postulate position-at : t → Nat → Position.t
    {-# COMPILE JS position-at = doc => n => doc.positionAt(Number(n)) #-}

    postulate offset-at : t → Position.t → ℕ
    {-# COMPILE JS offset-at = doc => pos => BigInt(doc.offsetAt(pos)) #-}

    postulate line-at : t → Nat → TextLine.t
    {-# COMPILE JS line-at = doc => n => doc.lineAt(Number(n)) #-}

    postulate file-name : t → String
    {-# COMPILE JS file-name = doc => doc.fileName #-}

    postulate open-path : String → IO t
    {-# COMPILE JS open-path = path => () => AgdaModeImports.vscode.workspace.openTextDocument(path) #-}

    postulate save : t → IO Bool
    {-# COMPILE JS save = doc => () => doc.save() #-}

module ExtensionContext where
  postulate t : Set

  postulate get : IO t
  {-# COMPILE JS get = async () => AgdaModeImports.context #-}

  postulate extension-uri : t → Uri.t
  {-# COMPILE JS extension-uri = ctx => ctx.extensionUri #-}

  postulate extension-path : t → String
  {-# COMPILE JS extension-path = ctx => ctx.extensionPath #-}

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

    postulate register : JSON → t → IO Disposable.t
    {-# COMPILE JS register = selector => provider => async () => {
      const disposable = AgdaModeImports.vscode.languages.registerDefinitionProvider(selector, provider);
      return { "_,_": y => y["_,_"](null, { "record": z => z["record"](disposable) }) };
    } #-}

  register : DocumentSelector.t → t → IO Disposable.t
  register selector t = Internal.register (DocumentSelector.encode selector) t
