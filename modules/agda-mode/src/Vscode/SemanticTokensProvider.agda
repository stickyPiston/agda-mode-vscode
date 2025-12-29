module Vscode.SemanticTokensProvider where

open import Prelude.List
open import Prelude.JSON
open import Prelude.Sigma
open import Prelude.Maybe hiding (_<$>_ ; _>>=_)
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

postulate Document CancellationToken SemanticTokens : Set

record SemanticToken : Set where field
    line char length token-type modifier : ℕ
open SemanticToken

record Show (A : Set) : Set where field
    show : A → String
open Show

record Enum (A : Set) : Set where field
    enumerate : List A
open Enum

data DefaultTokenType : Set where
    namespace class enum interface struct typeParameter type parameter : DefaultTokenType
    variable-type property enumMember decorator event function method macro-type label : DefaultTokenType
    comment string keyword number regexp operator : DefaultTokenType

instance
    ShowDefaultTokenType : Show DefaultTokenType
    ShowDefaultTokenType = record { show = λ where
        namespace → "namespace"
        class → "class" ; enum → "enum"
        interface → "interface"
        struct → "struct"
        typeParameter → "typeParameter"
        type → "type"
        parameter → "parameter"
        variable-type → "variable"
        property → "property"
        enumMember → "enumMember"
        decorator → "decorator"
        event → "event"
        function → "function"
        method → "method"
        macro-type → "macro"
        label → "label"
        comment → "comment"
        string → "string"
        keyword → "keyword"
        number → "number"
        regexp → "regexp"
        operator → "operator" }

    EnumDefaultTokenType : Enum DefaultTokenType
    EnumDefaultTokenType = record
        { enumerate = namespace ∷ class ∷ enum ∷ interface ∷ struct ∷ typeParameter ∷ type ∷ parameter
                    ∷ variable-type ∷ property ∷ enumMember ∷ decorator ∷ event ∷ function ∷ method ∷ macro-type ∷ label
                    ∷ comment ∷ string ∷ keyword ∷ number ∷ regexp ∷ operator ∷ []
        }

data DefaultModifier : Set where
    declaration definition readonly static deprecated abstract-mod async : DefaultModifier
    modification documentation defaultLibrary : DefaultModifier

instance
    ShowDefaultModifier : Show DefaultModifier
    ShowDefaultModifier = record
        { show = λ where
            declaration → "declaration"
            definition → "definition"
            readonly → "readonly"
            static → "static"
            deprecated → "deprecated"
            abstract-mod → "abstract"
            async → "async"
            modification → "modification"
            documentation → "documentation"
            defaultLibrary → "defaultLibrary"
        }

    EnumDefaultModifier : Enum DefaultModifier
    EnumDefaultModifier = record
        { enumerate = declaration ∷ definition ∷ readonly ∷ static ∷ deprecated ∷ abstract-mod
                    ∷ async ∷ modification ∷ documentation ∷ defaultLibrary ∷ []
        }

module Legend where
    postulate t : Set

    private postulate build' : List String → List String → vscode-api → t
    {-# COMPILE JS build' = types => mods => vscode => new vscode.SemanticTokensLegend(types, mods) #-}

    build :
        (TokenType Modifier : Set)
        ⦃ show-tt : Show TokenType ⦄ ⦃ enum-tt : Enum TokenType ⦄
        ⦃ show-mod : Show Modifier ⦄ ⦃ enum-mod : Enum Modifier ⦄ → vscode-api → t
    build TokenType Modifier ⦃ show-tt ⦄ ⦃ enum-tt ⦄ ⦃ show-mod ⦄ ⦃ enum-mod ⦄ =
        build' (map (show-tt .show) (enum-tt .enumerate)) (map (show-mod .show) (enum-mod .enumerate))

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
