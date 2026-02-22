module AgdaMode.Extension where

open import Effect.Monad
open import Effect.Applicative

open import Agda.Builtin.Unit
open import Data.Maybe
import Data.Maybe.Effectful as Maybe
open import Data.String
open import Data.Product
open import Data.Bool
open import Data.List as List hiding (_++_)
open import Agda.Builtin.Bool
open import Agda.Builtin.Nat
open import Function hiding (id)
open import Data.Monoid

import Data.IO as IO
open IO using (IO)
open import Data.JSON
open import Data.JSON.Decode
open import Data.Map
open import Node.Process

open import Level

open import AgdaMode.Extension.Highlighting
open import AgdaMode.Extension.Keymap

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

record InputModeBuffer : Set where field
  start-pos : Position.t
  buffer : String

IMB-combine : InputModeBuffer → InputModeBuffer → InputModeBuffer
IMB-combine record { start-pos = p₁ ; buffer = b₁ } record { start-pos = p₂ ; buffer = b₂ } =
  record { start-pos = p₁ ; buffer = b₁ ++ b₂ }

instance
  IMB-Semigroup : Semigroup InputModeBuffer
  IMB-Semigroup = record { _<>_ = IMB-combine }

record Model : Set where field
    panel : Maybe (Panel.t ⊤)
    status-bar-item : StatusBarItem.t
    input-mode-status-item : StatusBarItem.t
    agda : Maybe Process.t
    stdout-buffer : String
    current-doc : Maybe TextDocument.t
    loaded-files : StringMap.t (List Token)
    tokens-request-emitter : EventEmitter.t ⊤
    running-info : String
    input-mode-buffer : Maybe InputModeBuffer
    underline-decoration : DecorationType.t
    keymap : Trie
open Model

data Msg : Set where
  load-file-msg : Msg
  agda-stdout-update : Buffer.t → Msg
  tokens-request : IO.Ref.t (Maybe SemanticTokens.t) → Msg
  definition-request : TextDocument.t → Position.t → IO.Ref.t (Maybe Location.t) → Msg
  new-active-editor : Maybe TextEditor.t → Msg
  type-request : JSON → Msg

-- TODO: Handle unexpected closes
spawn-agda : (Msg → IO ⊤) → IO Process.t
spawn-agda update = do
  proc ← Process.spawn "agda" ("--interaction-json" ∷ "--colour=always" ∷ "--verbose=2" ∷ [])
  Process.on-data proc λ buf → update (agda-stdout-update buf)
  pure proc

init : Trie → IO Model
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
    ; input-mode-buffer = nothing
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

  register-command-with-args "type" λ args → update (type-request args)
    -- execute-command "default:type" (⊤ ∋ args)

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

handle-clear-highlighting : Model → ⊤ → IO Model
handle-clear-highlighting model tt = case model .current-doc of λ where
  (just doc) → pure record model { loaded-files = model .loaded-files [ TextDocument.file-name doc ]:= [] }
  nothing → pure model

record ConstraintPosition : Set where
  constructor mkPosition
  field col line pos : Nat

position-decoder : Decoder ConstraintPosition
position-decoder = mkPosition
  <$> required "col" nat
  <*> required "line" nat
  <*> required "pos" nat

record ConstraintRange : Set where
  constructor mkRange
  field start end : ConstraintPosition

range-decoder : Decoder ConstraintRange
range-decoder = mkRange
  <$> required "start" position-decoder
  <*> required "end" position-decoder

record Constraint : Set where
  constructor mkConstraint
  field
    id : Nat
    range : List ConstraintRange

constraint-decoder : Decoder Constraint
constraint-decoder = mkConstraint
  <$> required "id" nat
  <*> required "range" (list range-decoder)

record Goal : Set where
  constructor mkGoal
  field
    constraint : Constraint
    type : String

show-goal : Goal → String
show-goal (mkGoal (mkConstraint name _) type) = "?" ++ primShowNat name ++ " : " ++ type

goal-decoder : Decoder Goal
goal-decoder = mkGoal
  <$> required "constraintObj" constraint-decoder
  <*> required "type" string

data DisplayInfo : Set where
  all-goals-warnings :
    (errors : List String)
    (invisible-goals : List Goal)
    (visible-goals : List Goal)
    (warnings : List String) → DisplayInfo
  error : String → DisplayInfo

error-decoder : Decoder String
error-decoder = required "message" string

display-info-decoder : Decoder DisplayInfo
display-info-decoder = do
  "DisplayInfo" ← required "kind" string where _ → ⊘
  required "info" $
    required "kind" string >>= λ where
      "AllGoalsWarnings" → all-goals-warnings
        <$> required "errors" (list error-decoder)
        <*> required "invisibleGoals" (list goal-decoder)
        <*> required "visibleGoals" (list goal-decoder)
        <*> required "warnings" (list error-decoder)
      "Error" → error <$> required "error" (required "message" string)
      _ → ⊘

_when_ : A → Bool → List A
a when true = [ a ]
a when false = []

show-display-info : DisplayInfo → String
show-display-info (all-goals-warnings errors inv vis warns) =
  unlines $
               ("---------- Goals ----------\n" when not (null? (inv ⟨ append ⟩ vis)))
    ⟨ append ⟩ (map show-goal (List.append vis inv))
    ⟨ append ⟩ ("---------- Errors ----------\n" when not (null? errors))
    ⟨ append ⟩ errors
    ⟨ append ⟩ ("---------- Warnings ----------\n" when not (null? warns))
    ⟨ append ⟩ warns
show-display-info (error message) = message

open import Agda.Builtin.Equality

instance
  cloneable-⊤ : Cloneable ⊤
  cloneable-⊤ = record
    { encode = λ _ → j-null
    ; decode = λ { j-null → just tt ; _ → nothing }
    ; encode-decode-dual = λ { tt → refl }
    }

new-panel : IO (Panel.t ⊤)
new-panel = Panel.create
  "agdaMode-buffer"
  "*Agda information*"
  (record { preserve-focus = true ; view-column = ViewColumn.three })
  WebviewOptions.default

handle-display-info : Model → DisplayInfo → IO Model
handle-display-info model display-info = try λ _ → do
  panel ← from-Maybe new-panel (pure <$> model .panel)
  Panel.set-html panel ("<pre>" ++ show-display-info display-info ++ "</pre>")
  pure record model { panel = just panel }

record Status : Set where
  constructor mkStatus
  field checked show-implicit show-irrelevant : 𝔹
open Status

show-status : Status → String
show-status status = intercalate "," $
             ("Checked" when status .checked)
  ⟨ append ⟩ ("ShowImpl" when status .show-implicit)
  ⟨ append ⟩ ("ShowIrr" when status .show-irrelevant)

status-decoder : Decoder Status
status-decoder = do
  "Status" ← required "kind" string where _ → ⊘
  required "status" $ mkStatus
    <$> required "checked" bool
    <*> required "showImplicitArguments" bool
    <*> required "showIrrelevantArguments" bool

handle-status : Model → Status → IO Model
handle-status model status = do
  StatusBarItem.set-text (model .status-bar-item) (show-status status)
  StatusBarItem.show (model .status-bar-item)
  pure model

clear-running-info-decoder : Decoder ⊤
clear-running-info-decoder = required "kind" string >>= λ where
  "ClearRunningInfo" → pure tt ; _ → ⊘

handle-clear-running-info : Model → ⊤ → IO Model
handle-clear-running-info model tt = do
  panel ← from-Maybe new-panel (pure <$> model .panel)
  Panel.set-html panel ""
  pure record model { panel = just panel ; running-info = "" }

record RunningInfo : Set where
  constructor mkRunningInfo
  field
    debug-level : Nat
    message : String
open RunningInfo

running-info-decoder : Decoder RunningInfo
running-info-decoder = do
  "RunningInfo" ← required "kind" string where _ → ⊘
  mkRunningInfo <$> required "debugLevel" nat <*> required "message" string

handle-running-info : Model → RunningInfo → IO Model
handle-running-info model info = do
  panel ← from-Maybe new-panel (pure <$> model .panel)
  -- TODO: Allow configuration of debug level
  let new-running-info = model .running-info ++ info .message ++ "\n"
  Panel.set-html panel $ "<pre>" ++ new-running-info ++ "</pre>"
  pure record model { panel = just panel ; running-info = new-running-info }

record JumpToError : Set where
  constructor mkJumpToError
  field
    file-path : String
    position : Nat
open JumpToError

jump-to-error-decoder : Decoder JumpToError
jump-to-error-decoder = do
  "JumpToError" ← required "kind" string where _ → ⊘
  mkJumpToError <$> required "filepath" string <*> required "position" nat

handle-jump-to-error : Model → JumpToError → IO Model
handle-jump-to-error model jump-to-error = do
  doc ← TextDocument.open-path (jump-to-error .file-path)
  let uri = TextDocument.uri doc
  let pos = TextDocument.position-at doc (jump-to-error .position)
  Window.show-text-document uri record
    { preserve-focus = true
    ; preview = true
    ; selection = Range.new pos pos
    ; view-column = ViewColumn.active
    }
  pure model

handle-agda-message : Model → Decoder (IO Model)
handle-agda-message model =
  ⦇ (handle-highlighting-info model) highlighting-info-decoder
  | (handle-clear-highlighting model) clear-highlighting-decoder
  | (handle-display-info model) display-info-decoder
  | (handle-status model) status-decoder
  | (handle-clear-running-info model) clear-running-info-decoder
  | (handle-running-info model) running-info-decoder
  | (handle-jump-to-error model) jump-to-error-decoder
  ⦈

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

  (type-request o) → case o of λ where
    (j-object m) → case m !? "text" of λ where
      (just (j-string text)) → do
        execute-command "default:type" (j-object ("text" ↦ j-string text))
        new-model ← if text =~ "^\\s$"
          then ( -- Check for any whitespace character
            TextEditor.active-editor >>= λ where
              (just e) → case model .input-mode-buffer of λ where
                (just record { start-pos = start-pos }) → do
                  TextEditor.remove-decoration (model .underline-decoration) e
                  end-pos ← Position.left 1 <$> TextEditor.cursor-pos e
                  done ← TextEditor.edit [ Edit.replace (Range.new start-pos end-pos) "λ" ] e
                  StatusBarItem.hide (model .input-mode-status-item)
                  pure record model { input-mode-buffer = nothing }
                nothing → pure model
              nothing → pure model)
          else do
            if text =~ "^\\\\$"
              then (TextEditor.active-editor >>= λ where
                (just ed) → do
                  start-pos ← Position.left 1 <$> TextEditor.cursor-pos ed
                  let imb = record { start-pos = start-pos ; buffer = "" }
                  pure record model { input-mode-buffer = just imb }
                nothing   → pure model)
              else (case model .input-mode-buffer of λ where
                (just imb@record { buffer = b }) →
                  pure record model { input-mode-buffer = just (record imb { buffer = b ++ text }) }
                nothing → pure model)

        TextEditor.active-editor >>= λ where
          (just e) → case new-model .input-mode-buffer of λ where
            (just record { start-pos = start-pos ; buffer = b }) → do
              range ← Range.new start-pos <$> TextEditor.cursor-pos e
              e |> TextEditor.set-decoration (model .underline-decoration) range
              let next = join ∘ sort ∘ from-Maybe [] $ next-characters (split b) (model .keymap)
              StatusBarItem.set-text (model .input-mode-status-item) ("\\" ++ b ++ "[" ++ next ++ "]")
              StatusBarItem.show (model .input-mode-status-item)
              pure new-model
            nothing → pure new-model
          nothing → pure new-model
      _ → pure model
    _ → pure model

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
