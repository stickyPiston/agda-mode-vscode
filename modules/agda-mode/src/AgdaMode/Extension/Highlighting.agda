module AgdaMode.Extension.Highlighting where

open import Data.Bool
open import Data.List
open import Data.Product
open import Data.Nat hiding (_==_)
open import Data.IO
open import Data.Maybe using (Maybe ; just ; nothing)
open import Data.Maybe.Effectful
open import Agda.Builtin.Unit
open import Function
open import Effect.Monad
open Monad {{ ... }}
open TraversableM {{ ... }}

open import Vscode.Common
open import Vscode.SemanticTokensProvider
open import Vscode.TextEditor

open import AgdaMode.Extension.Highlighting.Decode
open import AgdaMode.Extension.Highlighting.Legend
open import AgdaMode.Extension.Position

apply-decorations : HighlightDecorationMap.t → TextEditor.t → List Token.t → IO ⊤
apply-decorations hd-map e tokens = do
  let modifiers = tokens |> concat-map λ token → map ((_, token .range) ∘ HighlightDecoration.from-SecondaryAspect) (token .secondary)
  doc ← TextEditor.document e
  forM HighlightDecoration.enum λ dec → do
    let ranges = modifiers |> map-Maybe λ (dec' , pos) → if dec' HighlightDecoration.== dec then just pos else nothing
    let vsc-ranges = ranges |> map (OffsetRange.to-vsc-range doc)
    TextEditor.set-decoration (hd-map dec) vsc-ranges e
  pure tt

make-highlighting-tokens : TextDocument.t → List Token.t → List SemanticToken.t
make-highlighting-tokens doc tokens = tokens
  |> map-Maybe (λ token → (_, token .range) <$> token .primary)
  |> map λ (aspect , range) → record
    { range = OffsetRange.to-vsc-range doc range
    ; token-type = TokenType.show (TokenType.from-PrimaryAspect aspect)
    ; modifiers = []
    }

DefaultLegend : Legend.t
DefaultLegend =
  record
    { TokenType = DefaultTokenType
    ; Modifier = DefaultModifier
    }

apply-semantic-tokens : TextDocument.t → List Token.t → IO SemanticTokens.t
apply-semantic-tokens doc tokens = do
  stb ← SemanticTokensBuilder.new =<< Legend.build DefaultLegend
  let semantic-tokens = make-highlighting-tokens doc tokens
  mapM (SemanticTokensBuilder.push stb) semantic-tokens
  SemanticTokensBuilder.build stb