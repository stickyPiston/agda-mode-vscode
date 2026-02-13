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
open import Function

import Data.IO as IO
open IO using (IO)
open import Data.JSON
open import Data.JSON.Decode
open import Data.Map
open import Node.Process

open import Level

open import AgdaMode.Extension.Highlighting

open import Vscode.Common
open import Vscode.Command
open import Vscode.Panel
open import Vscode.SemanticTokensProvider
open import Vscode.Window

postulate trace : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} → A → B → B
{-# COMPILE JS trace = _ => _ => _ => _ => a => b => {
  try { console.log(a); return b } catch (e) { console.error(e) }
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
    status-bar-item : StatusBarItem.t
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
init = try λ _ → do
  tokens-request-emitter ← EventEmitter.new
  sbi ← StatusBarItem.create "agdaMode.statusBar" StatusBarItem.right nothing
  pure record
    { panel = nothing
    ; status-bar-item = sbi
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

  -- TODO: Add listener to detect whether the buffer panel has been closed

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

handle-clear-highlighting : Model → IO Model
handle-clear-highlighting model = case model .current-doc of λ where
  (just doc) → pure record model { loaded-files = model .loaded-files [ TextDocument.file-name doc ]:= [] }
  nothing → pure model

record ConstraintPosition : Set where
  constructor mkPosition
  field col line pos : Nat

position-decoder : Decoder ConstraintPosition
position-decoder = mkPosition <$> nat <*> nat <*> nat

record ConstraintRange : Set where
  constructor mkRange
  field start end : ConstraintPosition

range-decoder : Decoder ConstraintRange
range-decoder = mkRange <$> position-decoder <*> position-decoder

record Constraint : Set where
  constructor mkConstraint
  field
    name : String
    range : ConstraintRange

constraint-decoder : Decoder Constraint
constraint-decoder = mkConstraint
  <$> required "name" string
  <*> required "range" range-decoder

record Goal : Set where
  constructor mkGoal
  field
    constraint : Constraint
    type : String

show-goal : Goal → String
show-goal (mkGoal (mkConstraint name _) type) = name ++ " : " ++ type

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

handle-agda-message : Model → Decoder (IO Model)
handle-agda-message model =
      (handle-highlighting-info model <$> highlighting-info-decoder)
  <|> (handle-clear-highlighting model <$ clear-highlighting-decoder)
  <|> (handle-display-info model <$> display-info-decoder)
  <|> (handle-status model <$> status-decoder)
  <|> ((λ x → traceM x >> pure model) <$> any)

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

  (tokens-request resolve reject) → case model .current-doc of λ where
    nothing → trace "Rejected tokens request" $ reject "Busy" >> pure model
    (just doc) → case model .loaded-files !? TextDocument.file-name doc of λ where
      (just tokens) → do
        let open TraversableA IO.Effectful.applicative
        stb ← SemanticTokensBuilder.new =<< Legend.build DefaultLegend
        let semantic-tokens = make-highlighting-tokens doc tokens
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
activate = try λ _ → do
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
