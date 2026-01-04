module Vscode.SemanticTokensProvider where

open import Prelude.List
open import Prelude.JSON
open import Prelude.Sigma
open import Prelude.Maybe hiding (_<$>_ ; _>>=_ ; pure)
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
open import Prelude.Vec hiding (map)

private postulate trace : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} → A → B → B
{-# COMPILE JS trace = _ => _ => _ => _ => thing => val => { console.log(thing) ; return val } #-}

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

postulate CancellationToken SemanticTokens : Set

DefaultTokenType DefaultModifier : Σ[ n ∈ ℕ ] Vec String n
DefaultTokenType = 23 , ("namespace" ∷ "class" ∷ "enum" ∷ "interface" ∷ "struct" ∷ "typeParameter" ∷ "type" ∷ "parameter"
      ∷ "variable" ∷ "property" ∷ "enumMember" ∷ "decorator" ∷ "event" ∷ "function" ∷ "method" ∷ "macro" ∷ "label"
      ∷ "comment" ∷ "string" ∷ "keyword" ∷ "number" ∷ "regexp" ∷ "operator" ∷ [])
DefaultModifier = 10 , ("declaration" ∷ "definition" ∷ "readonly" ∷ "static" ∷ "deprecated" ∷ "abstract" ∷ "async"
    ∷ "modification" ∷ "documentation" ∷ "defaultLibrary" ∷ [])

module Legend where
    record t : Set where field
        TokenType Modifier : Σ[ n ∈ ℕ ] Vec String n
    open t public

    postulate internal-t : Set

    private postulate build' : ∀ {n m} → Vec String n → Vec String m → vscode-api → internal-t
    {-# COMPILE JS build' = _ => _ => types => mods => vscode => new vscode.SemanticTokensLegend(types, mods) #-}

    build : t → vscode-api → internal-t
    build legend = build' (legend .TokenType .Σ.proj₂) (legend .Modifier .Σ.proj₂)

module Position where
    postulate t : Set
    postulate new : vscode-api → ℕ → ℕ → t
    postulate line char : t → ℕ

    {-# COMPILE JS new = vscode => line => char => new vscode.Position(Number(line), Number(char)) #-}
    {-# COMPILE JS line = pos => BigInt(pos.line) #-}
    {-# COMPILE JS char = pos => BigInt(pos.character) #-}

module Range where
    postulate t : Set
    postulate new : vscode-api → Position.t → Position.t → t
    postulate start end : t → Position.t

    {-# COMPILE JS new = vscode => start => end => new vscode.Range(start, end) #-}
    {-# COMPILE JS start = range => range.start #-}
    {-# COMPILE JS end = range => range.end #-}

module SemanticToken where
    open Legend.t
    record t : Set where field
        range : Range.t
        token-type : String
        modifiers : List String
    open t public

module TextLine where
    postulate t : Set

    postulate range : t → Range.t
    {-# COMPILE JS range = line => line.range #-}

module TextDocument where
    postulate t : Set

    postulate get-text : t → String
    {-# COMPILE JS get-text = doc => doc.getText() #-}

    postulate position-at : t → ℕ → Position.t
    {-# COMPILE JS position-at = doc => n => doc.positionAt(Number(n)) #-}

    postulate line-at : t → ℕ → TextLine.t
    {-# COMPILE JS line-at = doc => n => doc.lineAt(Number(n)) #-}

module SemanticTokensBuilder where
    postulate t : Set

    postulate new : vscode-api → Legend.internal-t → IO t
    {-# COMPILE JS new = vscode => legend => cont => cont(new vscode.SemanticTokensBuilder(legend)) #-}

    postulate build : t → IO SemanticTokens
    {-# COMPILE JS build = t => cont => cont(t.build()) #-}

    postulate push : t → Range.t → String → List String → IO ⊤
    {-# COMPILE JS push = t => r => tokenType => mod => cont => { t.push(r, tokenType, mod) ; cont(a => a["tt"]()) } #-}

-- TODO: Handle cancellations
postulate register-semantic-tokens-provider : vscode-api → JSON → EventEmitter.t → (TextDocument.t → CancellationToken → (return : SemanticTokens → IO ⊤) → IO ⊤) → Legend.internal-t → IO Disposable
{-# COMPILE JS register-semantic-tokens-provider = (vscode => selector => onChangeEmitter => provider => legend => cont => {
    cont(vscode.languages.registerDocumentSemanticTokensProvider(
        selector,
        {
            onDidChangeSemanticTokens: onChangeEmitter.event,
            provideDocumentSemanticTokens: (document, token) => new Promise((resolve, reject) => { provider(document)(token)(resolve)(() => {}) }),
        },
        legend
))}) #-}

semantic-tokens-provider : ∀ {msg} → Legend.t
    → (TextDocument.t → (List SemanticToken.t → Cmd msg) → msg)
    → LanguageFilter → Capability msg
semantic-tokens-provider {msg} legend on-request-msg selector = record
    { requirement-type = EventEmitter.t
    ; new-requirement = λ sys → EventEmitter.new (sys .vscode)
    ; provided-type = just (Cmd msg , λ on-change-emitter → Cmd.new λ  _ → EventEmitter.fire on-change-emitter)
    ; register = λ system requirement update →
        let vscode = system .vscode
            provider = λ doc token return → update $ on-request-msg doc λ tokens → Cmd.new λ _ → do
                builder ← SemanticTokensBuilder.new vscode (Legend.build legend vscode)
                forM tokens λ t → SemanticTokensBuilder.push builder
                    (t .SemanticToken.range)
                    (t .SemanticToken.token-type)
                    (t .SemanticToken.modifiers)
                SemanticTokensBuilder.build builder >>= return
         in just <$> register-semantic-tokens-provider vscode (encode-language-filter selector) requirement provider (Legend.build legend vscode)
    }
