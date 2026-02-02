module Vscode.SemanticTokensProvider where

open import Data.List
open import Data.Maybe using (Maybe ; just ; nothing)
open import Agda.Builtin.Nat
open import Data.String
open import Agda.Builtin.Unit
open import Data.Product
open import Data.Vec

open import Data.IO as IO

open import Vscode.Common

module Event where
  postulate t : Set → Set

  postulate listen : ∀ {A} → t A → (A → IO ⊤) → IO Disposable
  {-# COMPILE JS listen = _ => event => handler => (_, cont) => cont(event(handler)) #-}

module EventEmitter where
    postulate t : Set → Set

    postulate new : ∀ {A} → IO (t A)
    {-# COMPILE JS new = _ => ({vscode}, cont) => cont(new vscode.EventEmitter()) #-}

    postulate fire : ∀ {A} → t A → A → IO ⊤
    {-# COMPILE JS fire = _ => emitter => a => (_, cont) => { emitter.fire(a) ; cont(a => a["tt"]()) } #-}

    postulate event : ∀ {A} → t A → Event.t A
    {-# COMPILE JS event = _ => emitter => emitter.event #-}

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

module SemanticTokens where
    postulate t : Set

DefaultTokenType DefaultModifier : Σ Nat (Vec String)
DefaultTokenType = 23 , ("namespace" ∷ "class" ∷ "enum" ∷ "interface" ∷ "struct" ∷ "typeParameter" ∷ "type" ∷ "parameter"
      ∷ "variable" ∷ "property" ∷ "enumMember" ∷ "decorator" ∷ "event" ∷ "function" ∷ "method" ∷ "macro" ∷ "label"
      ∷ "comment" ∷ "string" ∷ "keyword" ∷ "number" ∷ "regexp" ∷ "operator" ∷ [])
DefaultModifier = 10 , ("declaration" ∷ "definition" ∷ "readonly" ∷ "static" ∷ "deprecated" ∷ "abstract" ∷ "async"
    ∷ "modification" ∷ "documentation" ∷ "defaultLibrary" ∷ [])

module Legend where
    record t : Set where field
        TokenType Modifier : Σ Nat (Vec String)
    open t public

    postulate internal-t : Set

    private postulate build' : ∀ {n m} → Vec String n → Vec String m → IO internal-t
    {-# COMPILE JS build' = _ => _ => types => mods => ({ vscode }, cont) => cont(new vscode.SemanticTokensLegend(types, mods)) #-}

    build : t → IO internal-t
    build legend = build' (legend .TokenType .proj₂) (legend .Modifier .proj₂)

module SemanticToken where
    open Legend.t
    record t : Set where field
        range : Range.t
        token-type : String
        modifiers : List String
    open t public

module SemanticTokensBuilder where
    postulate t : Set

    postulate new : Legend.internal-t → IO t
    {-# COMPILE JS new = legend => ({vscode}, cont) => cont(new vscode.SemanticTokensBuilder(legend)) #-}

    postulate build : t → IO SemanticTokens.t
    {-# COMPILE JS build = t => (_, cont) => cont(t.build()) #-}

    module Internal where
      postulate push : t → Range.t → String → List String → IO ⊤
      {-# COMPILE JS push = t => r => tokenType => mod => (_, cont) => { t.push(r, tokenType, mod) ; cont(a => a["tt"]()) } #-}

    push : t → SemanticToken.t → IO ⊤
    push t record { range = range ; token-type = token-type ; modifiers = modifiers } =
      Internal.push t range token-type modifiers

module Promise where
  postulate t : Set → Set → Set

  postulate new : ∀ {E A} → ((resolve : A → IO ⊤) → (reject : E → IO ⊤) → IO ⊤) → IO (t E A)
  {-# COMPILE JS new = _ => _ => b => (imports, cont) => new Promise((resolve, reject) => {
    b(resolve)(reject)(imports, _ => {});
  }) #-}

module SemanticTokensProvider where
  postulate t : Set
  -- private record t : Set₁ where field
  --   on-did-change-semantic-tokens : Maybe (Event.t ⊤)
  --   provide-document-semantic-tokens : ∀ {E} → TextDocument.t → CancellationToken.t → Promise.t E SemanticTokens.t

  postulate new : ∀ {E} → Maybe (Event.t ⊤) → (TextDocument.t → CancellationToken.t → IO (Promise.t E SemanticTokens.t)) → IO t
  {-# COMPILE JS new = _ => e => f => (imports, cont) => cont({
    onDidChangeSemanticTokens: e({ "nothing": () => undefined, "just": a => a }),
    provideDocumentSemanticTokens: (doc, token) => f(doc)(token)(imports, _ => {})
  }) #-}

  open import Data.JSON

  private postulate register' : JSON → t → Legend.internal-t → IO Disposable
  {-# COMPILE JS register' = selector => stp => legend => ({vscode}, cont) => {
    cont(vscode.languages.registerDocumentSemanticTokensProvider(selector, stp, legend))
  } #-}

  open import Effect.Monad
  open IO.Effectful
  open Monad ⦃ ... ⦄

  register : DocumentSelector.t → t → Legend.t → IO Disposable
  register selector stp legend = Legend.build legend >>= register' (DocumentSelector.encode selector) stp
