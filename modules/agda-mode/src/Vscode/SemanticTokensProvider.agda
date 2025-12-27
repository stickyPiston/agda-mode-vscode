module Vscode.SemanticTokensProvider where

open import Prelude.List
open import Prelude.JSON
open import Prelude.Sigma
open import Agda.Builtin.String
open import TEA.System
open import Iepje.Internal.JS.Language.IO
open import Agda.Builtin.Unit

postulate EventEmitter : Set

postulate new-event-emitter : IO EventEmitter

postulate fire : EventEmitter → IO ⊤

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

postulate Legend SemanticToken : Set

postulate mk-Legend : List String → List String → vscode-api → Legend
{-# COMPILE JS mk-Legend = types => mods => vscode => new vscode.SemanticTokensLegend(types, mods) #-}