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
  {-# COMPILE JS listen = _ => event => handler => async () => event(handler) #-}

module EventEmitter where
    postulate t : Set → Set

    postulate new : ∀ {A} → IO (t A)
    {-# COMPILE JS new = _ => async () => new AgdaModeImports.vscode.EventEmitter() #-}

    postulate fire : ∀ {A} → t A → A → IO ⊤
    {-# COMPILE JS fire = _ => emitter => a => async () => { emitter.fire(a) ; return a => a["tt"]() } #-}

    postulate event : ∀ {A} → t A → Event.t A
    {-# COMPILE JS event = _ => emitter => emitter.event #-}

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
    {-# COMPILE JS build' = _ => _ => types => mods => async () => new AgdaModeImports.vscode.SemanticTokensLegend(types, mods) #-}

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
    {-# COMPILE JS new = legend => async () => new AgdaModeImports.vscode.SemanticTokensBuilder(legend) #-}

    postulate build : t → IO SemanticTokens.t
    {-# COMPILE JS build = t => async () => t.build() #-}

    module Internal where
      postulate push : t → Range.t → String → List String → IO ⊤
      {-# COMPILE JS push = t => r => tokenType => mod => async () => { t.push(r, tokenType, mod) ; return a => a["tt"]() } #-}

    push : t → SemanticToken.t → IO ⊤
    push t record { range = range ; token-type = token-type ; modifiers = modifiers } =
      Internal.push t range token-type modifiers

module SemanticTokensProvider where
  postulate t : Set

  postulate new : Maybe (Event.t ⊤) → (TextDocument.t → CancellationToken.t → IO SemanticTokens.t) → IO t
  {-# COMPILE JS new = e => f => async () => ({
    onDidChangeSemanticTokens: e({ "nothing": () => undefined, "just": a => a }),
    provideDocumentSemanticTokens: (doc, token) => f(doc)(token)()
  }) #-}

  open import Data.JSON

  private postulate register' : JSON → t → Legend.internal-t → IO Disposable
  {-# COMPILE JS register' = selector => stp => legend => async () =>
    AgdaModeImports.vscode.languages.registerDocumentSemanticTokensProvider(selector, stp, legend) #-}

  open import Effect.Monad
  open IO.Effectful
  open Monad ⦃ ... ⦄

  register : DocumentSelector.t → t → Legend.t → IO Disposable
  register selector stp legend = Legend.build legend >>= register' (DocumentSelector.encode selector) stp
