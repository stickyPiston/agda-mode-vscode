module Vscode.SemanticTokensProvider where

open import Prelude.List
open import Prelude.JSON
open import Prelude.Sigma
open import Prelude.Maybe hiding (_<$>_)
open import Prelude.Nat
open import Agda.Builtin.String
open import TEA.System
open System
open import Iepje.Internal.JS.Language.IO
open import Agda.Builtin.Unit
open import TEA.Capability
import TEA.Cmd as Cmd
open import TEA.Cmd using (Cmd)
open import Iepje.Internal.Utils using (_$_ ; forM ; _>>_ ; _<$>_)

-- TODO: If there is a use for it, then allow event emitter to have a type parameter
module EventEmitter where
    postulate t : Set

    postulate new : vscode-api → IO t
    {-# COMPILE JS new = vscode => cont => cont(new vscode.EventEmitter()) #-}

    postulate fire : t → IO ⊤
    {-# COMPILE JS fire = emitter => cont => { emitter.fire() ; cont(a => a["tt"]()) } #-}

data LanguageFilter : Set where
    language scheme path-pattern : String → LanguageFilter
    _∩_ : LanguageFilter → LanguageFilter → LanguageFilter

encode-language-filter : LanguageFilter → JSON
encode-language-filter filter = j-object (kvs filter)
    where
        kvs : LanguageFilter → List (String × JSON)
        kvs (language x) = [ "language" , j-string x ]
        kvs (scheme x) = [ "scheme" , j-string x ]
        kvs (path-pattern x) = [ "pattern" , j-string x ]
        kvs (l ∩ r) = kvs l ++ kvs r

module Legend where
    postulate t : Set

    postulate new : List String → List String → vscode-api → t
    {-# COMPILE JS new = types => mods => vscode => new vscode.SemanticTokensLegend(types, mods) #-}

postulate Document CancellationToken SemanticTokens : Set

record SemanticToken : Set where field
    line char length token-type modifier : ℕ
open SemanticToken

-- ocaml-esque modules
module SemanticTokensBuilder where
    postulate t : Set

    postulate new : vscode-api → IO t
    {-# COMPILE JS new = vscode => cont => cont(new vscode.SemanticTokensBuilder()) #-}

    postulate build : t → IO SemanticTokens
    {-# COMPILE JS build = t => cont => cont(t.build()) #-}

    postulate push : t → (line char length token-type modifier : ℕ) → IO ⊤
    {-# COMPILE JS push = t => line => char => length => tokenType => mod => cont => { t.push(line, char, length, tokenType, mod) ; cont(a => a["tt"]()) } #-}

-- TODO: Handle cancellations
postulate register-semantic-tokens-provider : vscode-api → JSON → EventEmitter.t → (Document → CancellationToken → (return : SemanticTokens → IO ⊤) → IO ⊤) → Legend.t → IO Disposable
{-# COMPILE JS register-semantic-tokens-provider = vscode => selector => onChangeEmitter => provider => legend => vscode.languages.registerDocumentSemanticTokensProvider(
    selector,
    {
        onDidChangeSemanticTokens: onChangeEmitter.event,
        provideDocumentSemanticTokens: (document, token) => new Promise((resolve, reject) => { provider(document)(token)(resolve)(() => {}) }),
    },
    legend
) #-}

semantic-tokens-provider :
    ∀ {msg}
    → (vscode-api → Legend.t)
    → ((List SemanticToken → Cmd msg) → msg)
    → LanguageFilter
    → Capability msg
semantic-tokens-provider {msg} legend on-request-msg selector = record
    { requirement-type = EventEmitter.t
    ; new-requirement = λ sys → EventEmitter.new (sys .vscode)
    ; provided-type = just (Cmd msg , λ on-change-emitter → Cmd.new λ  _ → EventEmitter.fire on-change-emitter)
    ; register = λ system requirement update →
        let vscode = system .vscode
            provider = λ doc token return → update $ on-request-msg λ tokens → Cmd.new λ _ → do
                builder ← SemanticTokensBuilder.new vscode
                forM tokens λ t → SemanticTokensBuilder.push builder (t .line) (t .char) (t .length) (t .token-type) (t .modifier)
                SemanticTokensBuilder.build builder >>= return
         in just <$> register-semantic-tokens-provider vscode (encode-language-filter selector) requirement provider (legend vscode)
    }
