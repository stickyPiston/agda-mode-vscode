module AgdaMode.Extension where

open import Effect.Monad
open import Effect.Applicative

open import Agda.Builtin.Unit
open import Data.Maybe
import Data.Maybe.Effectful as Maybe
open import Data.String
open import Data.Product
open import Data.List hiding (_++_)
open import Agda.Builtin.Bool
open import Agda.Builtin.Nat
open import Function

import Data.IO as IO
open IO using (IO)
open import Data.JSON
open import Data.JSON.Decode
open import Data.Map

open import Level

open import AgdaMode.Extension.Highlighting

open import Vscode.Common
open import Vscode.Command
open import Vscode.StdioProcess
open import Vscode.Panel
open import Vscode.SemanticTokensProvider
open import Vscode.Window

postulate trace : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} → A → B → B
{-# COMPILE JS trace = _ => _ => _ => _ => a => b => {
  try { console.log(a); return b } catch (e) { console.error(e); throw e }
} #-}

traceM : ∀ {ℓ} {M : Set → Set ℓ} {A : Set} ⦃ m : Monad M ⦄ → A → M ⊤
traceM ⦃ m ⦄ a = trace a $ pure tt
  where open Monad m

postulate try : ∀ {ℓ} {A : Set ℓ} → (⊤ → A) → A
{-# COMPILE JS try = _ => _ => thing => {
  try { return thing(a => a["tt"]()) } catch (e) { console.error(e); throw e; }
} #-}

-- Actual app

private variable
  ℓ ℓ₁ ℓ₂ : Level
  M : Set ℓ₁ → Set ℓ₂
  A B : Set ℓ

open IO.Effectful

open Monad ⦃ ... ⦄
open MonadPlus ⦃ ... ⦄ using (⊘ ; _<|>_)

record Model : Set where field
    panel : Maybe (Panel.t ⊤)
    agda : Maybe Process.t
    stdout-buffer : String
    current-doc : Maybe TextDocument.t
    loaded-files : StringMap.t (List Token)
    tokens-request-emitter : EventEmitter.t ⊤
open Model

data Msg : Set where
  load-file-msg : Msg
  agda-stdout-update : Buffer.t → Msg
  tokens-request : (SemanticTokens.t → IO ⊤) → (String → IO ⊤) → Msg
  new-active-editor : Maybe TextEditor.t → Msg

-- TODO: Handle unexpected closes
spawn-agda : (Msg → IO ⊤) → IO Process.t
spawn-agda update = do
  proc ← Process.spawn "agda" [ "--interaction-json" ]
  Process.on-data proc λ buf → update (agda-stdout-update buf)
  pure proc

init : IO Model
init = do
  tokens-request-emitter ← EventEmitter.new
  pure record
    { panel = nothing
    ; stdout-buffer = ""
    ; current-doc = nothing
    ; loaded-files = StringMap.empty
    ; agda = nothing
    ; tokens-request-emitter = tokens-request-emitter
    }

DefaultLegend : Legend.t
DefaultLegend =
  record
    { TokenType = DefaultTokenType
    ; Modifier = DefaultModifier
    }

register : Model → (Msg → IO ⊤) → IO Model
register model update = do
  stp ← SemanticTokensProvider.new
    (just (EventEmitter.event $ model .tokens-request-emitter))
    λ doc token →
      Promise.new λ resolve reject →
        update (tokens-request resolve reject)
  SemanticTokensProvider.register
    (language "agda" ∩ scheme "file")
    stp
    DefaultLegend

  on-did-change-active-text-editor-listener λ editor → update (new-active-editor editor)
  current-doc ← TextEditor.active-editor

  register-command "agda-mode.load-file" (update load-file-msg)

  proc ← spawn-agda update

  pure record model
    { agda = just proc
    ; current-doc = _<$>_ {M = Maybe} TextEditor.document current-doc
    }

kind-decoder : Decoder String
kind-decoder = required "kind" string

parse-response : String → Maybe JSON
parse-response response = do
  let truncated-response = if (response starts-with "JSON> ") then slice 6 response else response
  parse-json truncated-response

ensure-process : (Msg → IO ⊤) → Model → IO Process.t
ensure-process update model = case model .agda of λ where
  nothing → spawn-agda update
  (just proc) → pure proc

foldM : ⦃ m : Monad M ⦄ → B → List A → (A → B → M B) → M B
foldM b [] f = pure b
foldM b (x ∷ xs) f = f x b >>= λ b' → foldM b' xs f

handle-highlighting-info : Model → List Token → IO Model
handle-highlighting-info model tokens = do
  EventEmitter.fire (model .tokens-request-emitter) tt
  case model .current-doc of λ where
    (just doc) → pure record model { loaded-files = model .loaded-files [ TextDocument.file-name doc ]:= tokens }
    nothing → pure model

clear-highlighting-decoder : Decoder ⊤
clear-highlighting-decoder =
  required "kind" string >>= λ where
    "ClearHighlighting" → pure tt
    _ → ⊘

handle-clear-highlighting : Model → IO Model
handle-clear-highlighting model = case model .current-doc of λ where
  (just doc) → pure record model { loaded-files = model .loaded-files [ TextDocument.file-name doc ]:= [] }
  nothing → pure model

handle-agda-message : Model → Decoder (IO Model)
handle-agda-message model =
      (handle-highlighting-info model <$> highlighting-info-decoder)
  <|> (handle-clear-highlighting model <$ clear-highlighting-decoder)

postulate print : {A : Set} → A → IO A
{-# COMPILE JS print = A => a => (_, cont) => { console.log(a); cont(a) } #-}

update : (Msg → IO ⊤) → Msg → Model → IO Model
update recurse msg model = trace msg $ case msg of λ where
  load-file-msg → case model .current-doc of λ where
    nothing → pure model
    (just doc) → do
      proc ← ensure-process recurse model
      let path = TextDocument.file-name doc
      Process.write proc ("IOTCM \"" ++ path ++ "\" NonInteractive Direct (Cmd_load \"" ++ path ++ "\" [])\n")
      pure model

  (agda-stdout-update buffer) → try λ _ →
    let ls = lines (model .stdout-buffer ++ Buffer.to-string buffer)
     in case unsnoc ls of λ where
        nothing → pure record model { stdout-buffer = "" } -- When the initial buffer is empty
        (just x) →
          let (responses , new-buffer) = x
              new-model = record model { stdout-buffer = new-buffer }
              open TraversableA Maybe.applicative
              parsed-responses = from-Maybe [] (mapA parse-response responses)
           in try λ _ → foldM new-model parsed-responses λ response model →
            from-Maybe (pure model) (handle-agda-message model response)

  (tokens-request resolve reject) → case model .current-doc of λ where
    nothing → trace "Rejected tokens request" $ reject "Busy" >> pure model
    (just doc) → case model .loaded-files !? TextDocument.file-name doc of λ where
      (just tokens) → do
        let open TraversableA IO.Effectful.applicative
        stb ← SemanticTokensBuilder.new =<< Legend.build DefaultLegend
        semantic-tokens ← make-highlighting-tokens doc tokens
        mapA (SemanticTokensBuilder.push stb) semantic-tokens
        built-tokens ← SemanticTokensBuilder.build stb
        resolve built-tokens
        pure model
      nothing → trace "Rejected tokens request" $ reject "Busy" >> pure model

  -- open-webview-msg → model , open-panel model system panel-opened received-webview
  -- (panel-opened panel) → record model { panel = just panel } , none

  (new-active-editor editor) → pure record model { current-doc = TextEditor.document <$> editor }

  -- (received-webview wmsg) → model , none

{-# TERMINATING #-}
activate : IO ⊤
activate = do
  model-ref ← init >>= IO.Ref.new
  m ← IO.Ref.get model-ref
  m' ← register m (update' model-ref)
  IO.Ref.set model-ref m'
  pure tt
  where
    update' : IO.Ref.t Model → Msg → IO ⊤
    update' model-ref msg = do
      m ← IO.Ref.get model-ref
      m' ← update (update' model-ref) msg m
      IO.Ref.set model-ref m'
      pure tt
