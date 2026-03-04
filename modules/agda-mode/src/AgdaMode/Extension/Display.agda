module AgdaMode.Extension.Display where

open import Data.String
open import Data.Nat
open import Data.IO
open import Data.List hiding (_++_)
open import Data.Maybe
open import Data.Maybe.Effectful
open import Data.Map
open import Data.Bool
open import Data.String renaming (∥_∥ to ∥_∥ˢ ; slice to sliceˢ)
open import Data.JSON
open import Data.JSON.Decode
open import Agda.Builtin.Unit

open import Function
open import Level

open import AgdaMode.Extension.Highlighting
open import AgdaMode.Extension.Model

open import Vscode.Window
open import Vscode.Panel
open import Vscode.Common
open import Vscode.TextEditor
open import Vscode.SemanticTokensProvider

open import Effect.Monad

open Monad ⦃ ... ⦄
open MonadPlus ⦃ ... ⦄ using (⊘ ; _<|>_)

private variable
  a : Level
  A : Set a

kind-decoder : Decoder String
kind-decoder = required "kind" string

parse-response : String → Maybe JSON
parse-response response = do
  let truncated-response = if (response starts-with "JSON> ") then sliceˢ 6 ∥ response ∥ˢ response else response
  parse-json truncated-response

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
    id' : Nat
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

_when'_ : A → 𝔹 → List A
a when' true = [ a ]
a when' false = []

show-display-info : DisplayInfo → String
show-display-info (all-goals-warnings errors inv vis warns) =
  unlines $
               ("---------- Goals ----------\n" when' not (null? (inv ⟨ append ⟩ vis)))
    ⟨ append ⟩ (map show-goal (append vis inv))
    ⟨ append ⟩ ("---------- Errors ----------\n" when' not (null? errors))
    ⟨ append ⟩ errors
    ⟨ append ⟩ ("---------- Warnings ----------\n" when' not (null? warns))
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
handle-display-info model display-info = do
  panel ← from-Maybe new-panel (pure <$> model .panel)
  Panel.set-html panel ("<pre>" ++ show-display-info display-info ++ "</pre>")
  pure record model { panel = just panel }

record Status : Set where
  constructor mkStatus
  field checked show-implicit show-irrelevant : 𝔹
open Status

show-status : Status → String
show-status status = intercalate "," $
             ("Checked" when' status .checked)
  ⟨ append ⟩ ("ShowImpl" when' status .show-implicit)
  ⟨ append ⟩ ("ShowIrr" when' status .show-irrelevant)

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
