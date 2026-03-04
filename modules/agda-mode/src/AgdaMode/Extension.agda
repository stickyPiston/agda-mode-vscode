module AgdaMode.Extension where

open import Effect.Monad
open import Effect.Applicative

open import Agda.Builtin.Unit
open import Data.Maybe
import Data.Maybe.Effectful as Maybe
open import Data.String renaming (∥_∥ to ∥_∥ˢ ; slice to sliceˢ)
open import Data.Product
open import Data.Bool
open import Data.List as List hiding (_++_) renaming (∥_∥ to ∥_∥ˡ ; slice to sliceˡ)
open import Agda.Builtin.Bool
open import Function
open import Data.Monoid
open import Data.Nat renaming (_==_ to _≡ⁿ_)

import Data.IO as IO
open IO using (IO ; when ; unless)
open import Data.JSON
open import Data.JSON.Decode
open import Data.Map
open import Node.Process

open import Level

open import AgdaMode.Extension.Highlighting
open import AgdaMode.Extension.Keymap
open import AgdaMode.Extension.Display
open import AgdaMode.Extension.Model

open import Vscode.Common
open import Vscode.Command
open import Vscode.Panel
open import Vscode.SemanticTokensProvider
open import Vscode.Window
open import Vscode.TextEditor

postulate trace : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} → A → B → B
{-# COMPILE JS trace = _ => _ => _ => _ => a => b => { console.log(a); return b } #-}

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

data Msg : Set where
  load-file-msg : Msg
  agda-stdout-update : Buffer.t → Msg
  tokens-request : IO.Ref.t (Maybe SemanticTokens.t) → Msg
  definition-request : TextDocument.t → Position.t → IO.Ref.t (Maybe Location.t) → Msg
  new-active-editor : Maybe TextEditor.t → Msg

-- TODO: Handle unexpected closes
spawn-agda : (Msg → IO ⊤) → IO Process.t
spawn-agda update = do
  proc ← Process.spawn "agda" ("--interaction-json" ∷ "--colour=always" ∷ "--verbose=2" ∷ [])
  Process.on-data proc λ buf → update (agda-stdout-update buf)
  pure proc

init : Trie.t → IO Model
init keymap = do
  tokens-request-emitter ← EventEmitter.new
  sbi₁ ← StatusBarItem.create "agdaMode.statusBar" StatusBarItem.right nothing
  sbi₂ ← StatusBarItem.create "agdaMode.inputMode" StatusBarItem.left nothing
  dec ←
    let open DecorationType
     in Options.new >>= create ∘ Options.set-text-decoration "underline"
  pure record
    { panel = nothing
    ; status-bar-item = sbi₁
    ; input-mode-status-item = sbi₂
    ; stdout-buffer = ""
    ; current-doc = nothing
    ; loaded-files = StringMap.empty
    ; agda = nothing
    ; tokens-request-emitter = tokens-request-emitter
    ; running-info = ""
    ; underline-decoration = dec
    ; keymap = keymap
    }

DefaultLegend : Legend.t
DefaultLegend =
  record
    { TokenType = DefaultTokenType
    ; Modifier = DefaultModifier
    }

postulate throw : ∀ {ℓ} {A : Set ℓ} {E : Set} → E → IO A
{-# COMPILE JS throw = _ => _ => _ => e => async () => { throw e } #-}

update-input-mode : Trie.t → DecorationType.t → StatusBarItem.t → IO.Ref.t InputMode.Model → InputMode.Msg → IO ⊤
update-input-mode t d stb input-mode-model msg = TextEditor.active-editor >>= λ where
  (just e) → IO.Ref.get input-mode-model >>= λ where
    (just m) → do
      model ← InputMode.update (t , e) (just m) msg
      model ← InputMode.view e d stb model
      IO.Ref.set input-mode-model model
    nothing → case msg of λ where
      (InputMode.character c) → do
        model ← InputMode.update (t , e) nothing c
        model ← InputMode.view e d stb model
        IO.Ref.set input-mode-model model
      _ → pure tt
  nothing → pure tt

register : Model → (Msg → IO ⊤) → IO Model
register model update = do
  stp ← SemanticTokensProvider.new
    (just (EventEmitter.event $ model .tokens-request-emitter))
    λ doc token → do
      r ← IO.Ref.new {A = Maybe SemanticTokens.t} nothing
      update (tokens-request r)
      IO.Ref.get r >>= λ where
        nothing → throw "Busy"
        (just tokens) → pure tokens

  SemanticTokensProvider.register (language "agda" ∩ scheme "file") stp DefaultLegend

  on-did-change-active-text-editor-listener λ editor → update (new-active-editor editor)
  current-doc ← TextEditor.active-editor

  register-command "agda-mode.load-file" (update load-file-msg)

  proc ← spawn-agda update

  -- TODO: Add listener to detect whether the buffer panel has been closed

  DefinitionProvider.register (language "agda" ∩ scheme "file") =<<
    DefinitionProvider.new λ doc pos tok → do
      r ← IO.Ref.new {A = Maybe Location.t} nothing
      update (definition-request doc pos r)
      IO.Ref.get r

  input-mode-model ← IO.Ref.new $ InputMode.Model ∋ nothing
  let uim = update-input-mode (model .keymap) (model .underline-decoration) (model .input-mode-status-item) input-mode-model
  register-command "agda-mode.backspace" $ uim InputMode.backspace
  register-command "agda-mode.arrow-left" $ uim InputMode.left
  register-command "agda-mode.arrow-right" $ uim InputMode.right
  register-command-with-args "type" λ args →
    required "text" (InputMode.character <$> string) args |> maybe (pure tt) uim

  pure record model
    { agda = just proc
    ; current-doc = _<$>_ {M = Maybe} TextEditor.document current-doc
    }

ensure-process : (Msg → IO ⊤) → Model → IO Process.t
ensure-process update model = case model .agda of λ where
  nothing → spawn-agda update
  (just proc) → pure proc

token-range : Nat → Nat → TextDocument.t → Range.t
token-range start end doc =
  TextDocument.position-at doc start ⟨ Range.new ⟩ TextDocument.position-at doc end

update : (Msg → IO ⊤) → Msg → Model → IO Model
update recurse msg model = trace msg $ case msg of λ where
  load-file-msg → case model .current-doc of λ where
    nothing → pure model
    (just doc) → do
      proc ← ensure-process recurse model
      let path = TextDocument.file-name doc
      Process.write proc ("IOTCM \"" ++ path ++ "\" NonInteractive Direct (Cmd_load \"" ++ path ++ "\" [])\n")
      pure model

  (agda-stdout-update buffer) →
    let ls = lines (model .stdout-buffer ++ Buffer.to-string buffer)
     in case unsnoc ls of λ where
        nothing → pure record model { stdout-buffer = "" } -- When the initial buffer is empty
        (just x) →
          let open TraversableM ⦃ ... ⦄
              open TraversableA ⦃ ... ⦄
              (responses , new-buffer) = x
              new-model = record model { stdout-buffer = new-buffer }
              parsed-responses = from-Maybe [] (mapA parse-response responses)
           in foldM new-model parsed-responses λ response model → do
            from-Maybe (pure model) (handle-agda-message model response)

  (tokens-request ref) → case model .current-doc of λ where
    nothing → pure model
    (just doc) → case model .loaded-files !? TextDocument.file-name doc of λ where
      (just tokens) → do
        let open TraversableA IO.Effectful.applicative
        stb ← SemanticTokensBuilder.new =<< Legend.build DefaultLegend
        let semantic-tokens = make-highlighting-tokens doc tokens
        mapA (SemanticTokensBuilder.push stb) semantic-tokens
        built-tokens ← SemanticTokensBuilder.build stb
        IO.Ref.set ref (just built-tokens)
        pure model
      nothing → pure model

  (new-active-editor editor) → pure record model { current-doc = TextEditor.document <$> editor }

  (definition-request doc pos ref) → case model .loaded-files !? TextDocument.file-name doc of λ where
    nothing → pure model -- Uhhh when does this actually happend?
    (just tokens) →
      let open Token
       in case find (λ tok → pos Range.in-range token-range (tok .start) (tok .end) doc) tokens of λ where
        (just token) → case token .definition-site of λ where
          nothing → pure model
          (just site) → do
            other ← TextDocument.open-path (site .filepath)
            let pos = TextDocument.position-at other (site .position - 1)
            IO.Ref.set ref (just $ Location.new (TextDocument.uri other) pos)
            pure model
        nothing → pure model

{-# TERMINATING #-}
activate : IO ⊤
activate = try λ _ →
  load-keymap "/Users/terra/Desktop/code/agda-mode-agda/modules/agda-mode/src/keymap.json" >>= λ where
    (just keymap) → do
      model-ref ← init keymap >>= IO.Ref.new
      m ← IO.Ref.get model-ref
      m' ← register m (update' model-ref)
      IO.Ref.set model-ref m'
      pure tt
    nothing → pure tt
  where
    update' : IO.Ref.t Model → Msg → IO ⊤
    update' model-ref msg = do
      m ← IO.Ref.get model-ref
      m' ← update (update' model-ref) msg m
      IO.Ref.set model-ref m'
      pure tt
