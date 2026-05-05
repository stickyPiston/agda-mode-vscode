module AgdaMode.Extension.Display where

open import Data.String hiding (show)
open import Data.Nat hiding (show ; _==_) ; import Data.Nat as Nat
open import Data.Int hiding (pos ; _+_)
open import Data.IO
import Data.IO as IO
open import Data.List hiding (any ; head) renaming (_++_ to _++ˡ_)
import Data.List as List
open import Data.Maybe
open import Data.Maybe.Effectful
open import Data.Map
open import Data.Bool
open import Data.String renaming (∥_∥ to ∥_∥ˢ ; slice to sliceˢ) hiding (show)
open import Data.JSON
open import Data.Product
open import Data.JSON.Decode
open import Agda.Builtin.Unit
open import Agda.Builtin.Char

open import Function hiding (id)
open import Level

open import AgdaMode.Extension.Highlighting
open import AgdaMode.Extension.Highlighting.Decode
open import AgdaMode.Extension.Model
open import AgdaMode.Extension.Position

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

private
  postulate trace : {A : Set} → A → IO ⊤
  {-# COMPILE JS trace = A => a => async () => { console.log(a); return b => b["tt"]() } #-}
  
  postulate try : ∀ {ℓ} {A : Set ℓ} → (⊤ → A) → A
  {-# COMPILE JS try = _ => _ => thing => {
    try { return thing(a => a["tt"]()) } catch (e) { console.error(e); throw e; }
  } #-}

kind-decoder : Decoder String
kind-decoder = required "kind" string

parse-response : String → Maybe JSON
parse-response response = do
  let truncated-response = if (response starts-with "JSON> ") then sliceˢ 6 ∥ response ∥ˢ response else response
  parse-json truncated-response

handle-highlighting-info : Model → List Token.t × Bool → IO Model
handle-highlighting-info model (token-list , remove) = do
  just e ← TextEditor.active-editor where _ → pure model
  doc ← TextEditor.document e

  m ← model .loaded-files !? TextDocument.file-name doc |> λ where
    nothing → pure record model
      { loaded-files = model .loaded-files [ TextDocument.file-name doc ]:= mkFile [] token-list
      }
    (just file) → pure record model
      { loaded-files = model .loaded-files [ TextDocument.file-name doc ]:=
          record file { tokens = (if remove then token-list else file .tokens ++ˡ token-list) }
      }
  
  EventEmitter.fire (model .tokens-request-emitter) tt
  
  pure m

clear-highlighting-decoder : Decoder ⊤
clear-highlighting-decoder =
  required "kind" string >>= λ where
    "ClearHighlighting" → pure tt
    _ → ⊘

handle-clear-highlighting : Model → ⊤ → IO Model
handle-clear-highlighting model tt = do
  just e ← TextEditor.active-editor where _ → pure model
  doc ← TextEditor.document e
  pure record model
    { loaded-files = model .loaded-files [ TextDocument.file-name doc ]:= mkFile [] []
    }

record ConstraintPosition : Set where
  constructor mkPosition
  field col line pos : Nat

position-decoder : Decoder ConstraintPosition
position-decoder = mkPosition
  <$> required "col" nat
  <*> required "line" nat
  <*> required "pos" nat

-- TODO: Isn't this just equivalent to Range.t?
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
show-goal (mkGoal (mkConstraint name _) type) = "?" ++ Nat.show name ++ " : " ++ type

goal-decoder : Decoder Goal
goal-decoder = mkGoal
  <$> required "constraintObj" constraint-decoder
  <*> required "type" string

record InvisibleConstraint : Set where
  constructor mkInvisibleConstraint
  field
    name : String
    range : List ConstraintRange
open InvisibleConstraint

invisible-constraint-decoder : Decoder InvisibleConstraint
invisible-constraint-decoder = mkInvisibleConstraint
  <$> required "name" string
  <*> required "range" (list range-decoder)

record InvisibleGoal : Set where
  constructor mkInvisibleGoal
  field
    constraint : InvisibleConstraint
    kind type : String
open InvisibleGoal

show-invisible-goal : InvisibleGoal → String
show-invisible-goal (mkInvisibleGoal constraint kind type) = constraint .name ++ " : " ++ type

invisible-goal-decoder : Decoder InvisibleGoal
invisible-goal-decoder = mkInvisibleGoal
  <$> required "constraintObj" invisible-constraint-decoder
  <*> required "kind" string
  <*> required "type" string

record ContextItem : Set where
  constructor mkContextItem
  field
    binding original-name reified-name : String
    in-scope? : Bool
open ContextItem

context-item-decoder : Decoder ContextItem
context-item-decoder = ⦇ mkContextItem
  (required "binding" string)
  (required "originalName" string)
  (required "reifiedName" string)
  (required "inScope" bool) ⦈

show-context-item : ContextItem → String
show-context-item item = item .original-name ++ " : " ++ item .binding ++ (if item .in-scope? then "" else " (not in scope)")

record Context : Set where
  constructor mkContext
  field
    context-items : List ContextItem
    interaction-point : InteractionPoint.t
open Context

show-context : Context → String
show-context ctx = ctx .context-items
  |> map show-context-item
  |> intercalate "\n"

data TypeAux : Set where
  goal-only : TypeAux
  goal-and-have : String → TypeAux
  goal-and-elaboration : String → TypeAux

type-aux-decoder : Decoder TypeAux
type-aux-decoder = required "kind" string >>= λ where
  "GoalOnly" → succeed goal-only
  "GoalAndHave" → goal-and-have <$> required "expr" string
  "GoalAndElaboration" → goal-and-elaboration <$> required "term" string
  _ → ⊘

show-aux : TypeAux → String
show-aux goal-only = ""
show-aux (goal-and-have expr) = "\nHave: " ++ expr
show-aux (goal-and-elaboration expr) = "\nElaborated: " ++ expr

module GoalInfo where
  data t : Set where
    inferred-type : String → t
    current-goal : Rewrite.t → String → t
    normal-form : ComputeMode.t → String → t
    goal-type : Maybe (List ContextItem) → String → TypeAux → t

  decoder : Decoder t
  decoder = required "kind" string >>= λ where
    "InferredType" → (| inferred-type (required "expr" string) |)
    "CurrentGoal" → (| current-goal (required "rewrite" Rewrite.decoder) (required "type" string) |)
    "GoalType" → (| goal-type (optional "entries" (list context-item-decoder)) (required "type" string) (required "typeAux" type-aux-decoder) |)
    "NormalForm" → (| normal-form (required "computeMode" ComputeMode.decoder) (required "expr" string) |)
    _ → ⊘

module ModuleContents where
  record t : Set where
    constructor mkModuleContents
    field
      names : List String
      contents : List (String × String)
      -- TODO: telescope
  open t public

  contents-item-decoder : Decoder (String × String)
  contents-item-decoder = (| required "name" string , required "term" string |)

  decoder : Decoder t
  decoder = (| mkModuleContents (required "names" (list string)) (required "contents" (list contents-item-decoder)) |)

open ModuleContents using (mkModuleContents ; names ; contents) public

data DisplayInfo : Set where
  all-goals-warnings :
    (errors : List String)
    (invisible-goals : List InvisibleGoal)
    (visible-goals : List Goal)
    (warnings : List String) → DisplayInfo
  error why-in-scope : String → DisplayInfo
  context : Context → DisplayInfo
  -- goal-info : InteractionPoint.t → GoalInfo → DisplayInfo
  intro-not-found : DisplayInfo
  goal-specific : GoalInfo.t → InteractionPoint.t → DisplayInfo
  module-contents : ModuleContents.t → DisplayInfo

error-decoder : Decoder String
error-decoder = required "message" string

ip-range-decoder : Decoder OffsetRange.t
ip-range-decoder = do
  start ← required "start" (required "pos" nat |> fmap (_- 1))
  end ← required "end" (required "pos" nat |> fmap (_- 1))
  pure $ offset-range start (end - start)

interaction-point-decoder : Decoder InteractionPoint.t
interaction-point-decoder = ⦇ mkInteractionPoint (required "id" nat) (list ip-range-decoder |> index 0 |> required "range") ⦈

context-decoder : Decoder Context
context-decoder = mkContext
  <$> required "context" (list context-item-decoder)
  <*> required "interactionPoint" interaction-point-decoder

data OutputConstraint : Set where
  of-type : (constraint-obj type : String) → OutputConstraint
  cmp-in-type : (comparison type lhs rhs : String) → OutputConstraint
  cmp-elim : (polarities : List String) (type : String) (lhs rhs : List String) → OutputConstraint
  just-type just-sort : (constraint-obj : String) → OutputConstraint
  cmp-types cmp-levels cmp-teles cmp-sorts : (comparison lhs rhs : String) → OutputConstraint
  assign : (constraint-obj value : String) → OutputConstraint
  typed-assign : (constraint-obj value type : String) → OutputConstraint
  postponed-check-args : (constraint-obj of-type type : String) (arguments : List String) → OutputConstraint
  is-empty-type size-lt-sat : (type : String) → OutputConstraint
  find-instance-of : (constraint-obj type : String) (candidates : List (String × String)) → OutputConstraint
  resolve-instance-of : (name : String) → OutputConstraint
  pts-instance : (lhs rhs : String) → OutputConstraint
  postponed-check-fun-def : (name type error : String) → OutputConstraint
  data-sort : (name sort : String) → OutputConstraint
  check-lock : (head lock : String) → OutputConstraint
  usable-at-mod : (mod term : String) → OutputConstraint

pair-decoder : Decoder A → Decoder (A × A)
pair-decoder d = ⦇ index 0 (list d) , index 1 (list d) ⦈

cmp-decoder : (String → String → String → OutputConstraint) → Decoder OutputConstraint
cmp-decoder c = ⦇ uncurry ⦇ c (required "comparison" string) ⦈ (required "constraintObjs" (pair-decoder string)) ⦈

output-constraint-decoder : Decoder OutputConstraint
output-constraint-decoder = required "kind" string >>= λ where
  "OfType" → ⦇ of-type (required "constraintObj" string) (required "type" string) ⦈
  "CmpInType" → ⦇ uncurry ⦇ cmp-in-type (required "comparison" string) (required "type" string) ⦈
                          (required "constraintObjs" (pair-decoder string)) ⦈
  "CmpElim" → ⦇ uncurry ⦇ cmp-elim (required "polarities" (list string)) (required "type" string) ⦈
                        (required "constraintObjs" (pair-decoder $ list string)) ⦈
  "JustType" → ⦇ just-type (required "constraintObj" string) ⦈
  "JustSort" → ⦇ just-sort (required "constraintObj" string) ⦈
  "CmpTypes" → cmp-decoder cmp-types ; "CmpLevels" → cmp-decoder cmp-levels
  "CmpTeles" → cmp-decoder cmp-teles ; "CmpSorts" → cmp-decoder cmp-sorts
  _ → ⊘

display-info-decoder : Decoder DisplayInfo
display-info-decoder = do
  "DisplayInfo" ← required "kind" string where _ → ⊘
  required "info" $
    required "kind" string >>= λ where
      "AllGoalsWarnings" → all-goals-warnings
        <$> required "errors" (list error-decoder)
        <*> required "invisibleGoals" (list invisible-goal-decoder)
        <*> required "visibleGoals" (list goal-decoder)
        <*> required "warnings" (list error-decoder)
      "Error" → error <$> required "error" (required "message" string)
      "Context" → context <$> context-decoder
      "GoalSpecific" → goal-specific
        <$> required "goalInfo" GoalInfo.decoder
        <*> required "interactionPoint" interaction-point-decoder
      "IntroNotFound" → succeed intro-not-found
      "ModuleContents" → (| module-contents ModuleContents.decoder |)
      "WhyInScope" → (| why-in-scope (required "message" string) |)
      _ → ⊘

_when'_ : A → Bool → List A
a when' true = [ a ]
a when' false = []

show-display-info : DisplayInfo → String
show-display-info (all-goals-warnings errors inv vis warns) =
  let content = 
        unlines $
                    ("---------- Goals ----------" when' not (null? vis))
          ⟨ append ⟩ (map show-goal vis)
          ⟨ append ⟩ (map show-invisible-goal inv)
          ⟨ append ⟩ ("\n---------- Errors ----------" when' not (null? errors))
          ⟨ append ⟩ errors
          ⟨ append ⟩ ("\n---------- Warnings ----------\n" when' not (null? warns))
          ⟨ append ⟩ warns
   in if content == "" then "All good." else content
show-display-info (error message) = message
show-display-info (context ctx) = show-context ctx
show-display-info (goal-specific (GoalInfo.inferred-type type) ip) = type
show-display-info (goal-specific (GoalInfo.normal-form _ nf) ip) = nf
show-display-info (goal-specific (GoalInfo.current-goal _ type) ip) =
  "?" ++ Nat.show (ip .id) ++ " : " ++ type
-- show-display-info (goal-info _ info) =
--   let context-info = info .entries |> maybe "" (("\n----- Context ---------------------------\n" ++_) ∘ intercalate "\n" ∘ map show-context-item) in
--   let aux-info = show-aux (info .type-aux) in
--   "Goal: " ++ info .type ++ aux-info ++ context-info
show-display-info (goal-specific _ _) = ""
show-display-info intro-not-found = "No introduction forms found."
show-display-info (module-contents (mkModuleContents names contents)) =
  "Modules\n" ++ intercalate "\n" (map ("  " ++_) names) ++
    "\nNames" ++ intercalate "\n" (map (λ (name , term) → "  " ++ name ++ " : " ++ term) contents)
show-display-info (why-in-scope message) = message

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
  field checked show-implicit show-irrelevant : Bool
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

-- TODO: The order of the interaction point list does not matter, so we might as well cons the ips instead of
-- inefficiently snoc'ing them.
expand-interaction-point : List InteractionPoint.t × Nat → InteractionPoint.t → List InteractionPoint.t × Nat
expand-interaction-point (ac , Δ) ip =
  if ip .range .length > 1 then
    ac List.++ [ ip ] , Δ
  else
    ac List.++ [ record ip { range = record (ip .range) { start = ip .range .start + Δ ; length = 6 } } ] , Δ + 5

-- This function only merges 1-wide interaction points with old interactions points. It should not occur that
-- interaction overlap in any other way.
merge-ip : List InteractionPoint.t → InteractionPoint.t → InteractionPoint.t
merge-ip old-ips ip =
  let old-ip = find (λ expanded-ip → OffsetRange.contains? (expanded-ip .range) (ip .range .start)) old-ips in
  (ip .range .length , old-ip) |> λ where
    (1 , just old-ip) → old-ip
    (_ , _) → ip

handle-interaction-points : Model → List InteractionPoint.t → IO Model
handle-interaction-points model ips = TextEditor.active-editor >>= maybe (pure model) λ e → do
  doc ← TextEditor.document e
  just (mkFile old-ips _) ← pure (model .loaded-files !? TextDocument.file-name doc) where _ → pure model

  -- Agda can respond with 0-wide interaction points in give and refinement interactioans, but we cannot extract
  -- any useful information from them (e.g. they have non-sensical and overlapping positions). Therefore, we just
  -- ignore them and reload the file afterwards to get more relevant information about new goals.
  let ips = ips
        |> filter (λ ip → ip .range .length > 0)
  
  -- After a refinement, Agda may send 1-wide interaction points at places where originally already
  -- expanded interaction points were. We try to merge the 1-wide goals with the previous goals to
  -- make sure that the previous goals will not be broken by new hole digs introduced by these faultily-sized
  -- goals.
        |> map (merge-ip old-ips)

  -- Dig the remaining 1-wide goals, this involves updating the goal cache in the model as well as
  -- performing edits in the buffer. In case of multiple digs, the edits send the current positions
  -- of the question marks and vscode shifts them internally. Whereas the goal cache requires that
  -- newly dug holes are shifted to their correct position after the edits are performed. This is because
  -- the change event handler will not shift edits to goals that fall exactly onto goals according to the cache,
  -- while still shifting the tokens and goals that are not recently dug in the regular fashion.
  let expanded-ips = ips |> foldl ([] , 0) expand-interaction-point |> Σ.fst
  let edits = ips |> map-Maybe λ ip → if ip .range .length ≤ 1
        then just (Edit.replace (OffsetRange.to-vsc-range doc (ip .range)) "{!  !}")
        else nothing

  TextEditor.edit edits e
  TextDocument.save doc
  doc ← TextEditor.document e

  -- If a new hole has been bug, place the cursor in the middle of the first new goal
  ips |> find (λ ip → ip .range .length ≤ 1) |> maybe (pure tt) λ ip →
    let pos = TextDocument.position-at doc (ip .range .start + 3) in
    TextEditor.set-selections [ Selection.new pos pos ] e

  model .loaded-files !? TextDocument.file-name doc
    |> (λ where
      (just file) → record file { interaction-points = expanded-ips }
      nothing → mkFile expanded-ips [])
    |> model .loaded-files [ TextDocument.file-name doc ]:=_
    |> (λ files → record model { loaded-files = files })
    |> pure

interaction-points-decoder : Decoder (List InteractionPoint.t)
interaction-points-decoder = do
  "InteractionPoints" ← required "kind" string where _ → ⊘
  required "interactionPoints" $ list interaction-point-decoder

module GiveResult where
  data t : Set where
    parens no-parens : t
    str : String → t

  show : t → String
  show (str s) = s
  show _ = ""
open GiveResult using (parens ; no-parens ; str) public

give-result-decoder : Decoder GiveResult.t
give-result-decoder = ⦇ if required "paren" bool then succeed parens else succeed no-parens | str (required "str" string) ⦈

record GiveAction : Set where
  constructor mkGiveAction
  field
    give-result : GiveResult.t
    interaction-point : InteractionPoint.t
open GiveAction

give-action-decoder : Decoder GiveAction
give-action-decoder = do
  "GiveAction" ← required "kind" string where _ → ⊘
  mkGiveAction <$> required "giveResult" give-result-decoder <*> required "interactionPoint" interaction-point-decoder

handle-give-action : (AgdaInteraction.t → IO ⊤) → Model → GiveAction → IO Model
handle-give-action send-command model give = TextEditor.active-editor >>= maybe (pure model) λ e → do
  doc ← TextEditor.document e
  let ip-range = OffsetRange.to-vsc-range doc (give .interaction-point .range)
      -- TODO: Change get-text to be IO
  let content = trim $ TextDocument.get-text (OffsetRange.to-vsc-range doc $ InteractionPoint.content-range (give .interaction-point)) doc
  let edits = give .give-result |> λ where
        parens → [ Edit.replace ip-range ("(" ++ content ++ ")") ]
        no-parens → [ Edit.replace ip-range content ]
        (str s) → [ Edit.replace ip-range s ]
  TextEditor.edit edits e
  TextDocument.save doc
  model <$ send-command (iotcm doc AgdaCommand.load)

data MakeCaseVariant : Set where
  function extlam : MakeCaseVariant

make-case-variant-decoder : Decoder MakeCaseVariant
make-case-variant-decoder = string >>= λ
  { "Function" → succeed function ; "ExtendedLambda" → succeed extlam ; _ → ⊘ }

record MakeCase : Set where
  constructor mkMakeCase
  field
    clauses : List String
    ip : InteractionPoint.t
    variant : MakeCaseVariant

make-case-decoder : Decoder MakeCase
make-case-decoder = do
  "MakeCase" ← required "kind" string where _ → ⊘
  ⦇ mkMakeCase
    (required "clauses" (list string))
    (required "interactionPoint" interaction-point-decoder)
    (required "variant" make-case-variant-decoder) ⦈

handle-make-case : (AgdaInteraction.t → IO ⊤) → Model → MakeCase → IO Model
handle-make-case send-command model (mkMakeCase clauses ip variant) = do
  just e ← TextEditor.active-editor where _ → pure model
  doc ← TextEditor.document e
  
  -- We replace the entire line the interaction point is located on. While this is not correct,
  -- we can't really do better than this. The compiler doesn't tell us where the clause is that
  -- we need to replace.
  --
  -- NOTE: This bug also exists in the emacs-mode.
  --   before        after
  -- ```agda     | ```agda
  -- f =         | f =
  --   {!  !}    |   f x = ?
  -- ```         | ```
  let pos = TextDocument.position-at doc (ip .range .start)
  let line = TextDocument.line-at doc (Position.line pos)
  
  -- Save the indentation of the line we are replacing, so that we can restore it later.
  let indentation = primStringFromList $ take-while primIsSpace (primStringToList $ TextLine.text line)

  TextEditor.edit [ Edit.replace (TextLine.range line) (intercalate "\n" $ map (indentation ++_) clauses) ] e 
  TextDocument.save doc
  
  -- We need to issue a reload because Agda sends interaction points that, for some reason,
  -- have already been expanded by the compiler. This means that the interaction points message
  -- that follows the make case message, will not expand the question marks that we have inserted
  -- here, since Agda sends 6-wide ranges instead of 1-wide at the places the questions marks are
  -- located.
  --
  -- NOTE: Fixing this would require the compiler to not expand the question marks. This would
  -- be more consistent with the interaction points messages that refine sends too.
  model <$ send-command (iotcm doc AgdaCommand.load)

-- TODO: Change this to have type Decoder (Model → IO Model)
handle-agda-message : (AgdaInteraction.t → IO ⊤) → Model → Decoder (IO Model)
handle-agda-message send-command model =
  ⦇ (handle-highlighting-info model) highlighting-info-decoder
  | (handle-clear-highlighting model) clear-highlighting-decoder
  | (handle-display-info model) display-info-decoder
  | (handle-status model) status-decoder
  | (handle-clear-running-info model) clear-running-info-decoder
  | (handle-running-info model) running-info-decoder
  | (handle-jump-to-error model) jump-to-error-decoder
  | (handle-interaction-points model) interaction-points-decoder
  | (handle-give-action send-command model) give-action-decoder
  | (handle-make-case send-command model) make-case-decoder
  -- | (λ x → trace x $ pure model) any
  ⦈
