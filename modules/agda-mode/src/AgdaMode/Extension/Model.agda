module AgdaMode.Extension.Model where

open import Agda.Builtin.Unit

open import Data.Maybe
open import Data.String hiding (show)
open import Data.Map
open import Data.List hiding (_++_)
open import Data.Product
open import Data.Nat renaming (show to show-Nat)
import Data.Nat as Nat
open import Data.Bool
open import Function hiding (id)
open import Data.IO
open import Effect.Monad
open Monad {{ ... }}

open import AgdaMode.Extension.Highlighting
open import AgdaMode.Extension.Keymap
open import AgdaMode.Extension.Goals
open import AgdaMode.Extension.Position

open import Vscode.Panel
open import Vscode.Window
open import Vscode.Common
open import Vscode.SemanticTokensProvider
open import Vscode.TextEditor

open import Node.Process

module InteractionPoint where
  record t : Set where
    constructor mkInteractionPoint
    field
      id : Nat
      range : OffsetRange.t
  open t public

  content-range : t → OffsetRange.t
  content-range (mkInteractionPoint _ range) = offset-range (range .start + 2) (range .length - 4)

open InteractionPoint using (mkInteractionPoint ; id ; range) public

record File : Set where
  constructor mkFile
  field
    interaction-points : List InteractionPoint.t
    tokens : List Token.t
open File public

record Model : Set where field
  panel : Maybe (Panel.t ⊤)
  status-bar-item : StatusBarItem.t
  input-mode-status-item : StatusBarItem.t
  agda : Maybe Process.t
  stdout-buffer : String
  current-doc : Maybe TextDocument.t
  loaded-files : StringMap.t File
  tokens-request-emitter : EventEmitter.t ⊤
  running-info : String
  underline-decoration ip-decoration : DecorationType.t
  keymap : Trie.t
open Model public

module Rewrite where
  data t : Set where
    as-is instantiated head-normal simplified normalised : t

  show : t → String
  show as-is = "AsIs"
  show instantiated = "Instantiated"
  show head-normal = "HeadNormal"
  show simplified = "Simplified"
  show normalised = "Normalised"
open Rewrite hiding (t ; show) public

module AgdaCommand where
  data t : Set where
    load : t
    -- Give uses the boolean to indicate whether force should be used,
    -- refine uses the boolean to indicate whether the compiler should create case lambdas when intro'ing functions
    give refine-or-intro : Bool → InteractionPoint.t → t
    context goal-type infer goal-type-context goal-type-context-infer goal-type-context-check auto-goal
      : Rewrite.t → InteractionPoint.t → t
    show-metas show-constraints : Rewrite.t → t
    make-case : InteractionPoint.t → t


  show-pos : Nat → TextDocument.t → String
  show-pos offset doc =
    let pos = TextDocument.position-at doc offset
     in "Pn () " ++ Nat.show (offset + 1) ++ " " ++ Nat.show (Position.line pos) ++ " " ++ Nat.show (Position.char pos)

  show-range : TextDocument.t → OffsetRange.t → String
  show-range doc (offset-range start length) =
    let range = "Interval () (" ++ show-pos start doc ++ ") (" ++ show-pos (start + length) doc ++ ")"
     in "intervalsToRange Nothing [" ++ range ++ "]"

  show-goal-command : TextDocument.t → InteractionPoint.t → List String
  show-goal-command doc ip = 
    let goal-content = doc |> TextDocument.get-text (OffsetRange.to-vsc-range doc $ InteractionPoint.content-range ip) in

    -- We send along an up-to-date version of the interaction point's range.
    -- Agda uses this range to update its internal state, and return a correct range when responding with a
    -- give action. This means we can fully rely on the new locations provided by Agda without consulting
    -- our own cache again.
    --
    -- NOTE: There does seem to be a bug/inconsistency in the compiler when sending the range including markers,
    -- where the locations of errors within the hole are reported incorrectly in the display infos.
    show-Nat (ip .id) ∷ ("(" ++ show-range doc (ip .range) ++ ")") ∷ ("\"" ++ goal-content ++ "\"") ∷ []

  show-goal-rewrite-command : TextDocument.t → Rewrite.t → InteractionPoint.t → List String
  show-goal-rewrite-command doc r ip = Rewrite.show r ∷ show-goal-command doc ip

  show-list : TextDocument.t → t → List String
  show-list doc load = "Cmd_load" ∷ "\"" ++ TextDocument.file-name doc ++ "\"" ∷ "[]" ∷ []
  show-list doc (give with-force ip) =
    let force = if with-force then "WithForce" else "WithoutForce" in
    "Cmd_give" ∷ force ∷ show-goal-command doc ip
  show-list doc (refine-or-intro b ip) =
    let b' = if b then "True" else "False" in
    "Cmd_refine_or_intro" ∷ b' ∷ show-goal-command doc ip
  show-list doc (context r ip) = "Cmd_context" ∷ show-goal-rewrite-command doc r ip
  show-list doc (goal-type r ip) = "Cmd_goal_type" ∷ show-goal-rewrite-command doc r ip
  show-list doc (infer r ip) = "Cmd_infer" ∷ show-goal-rewrite-command doc r ip
  show-list doc (goal-type-context r ip) = "Cmd_goal_type_context" ∷ show-goal-rewrite-command doc r ip
  show-list doc (goal-type-context-infer r ip) = "Cmd_goal_type_context_infer" ∷ show-goal-rewrite-command doc r ip
  show-list doc (goal-type-context-check r ip) = "Cmd_goal_type_context_check" ∷ show-goal-rewrite-command doc r ip
  show-list doc (auto-goal r ip) = "Cmd_autoOne" ∷ show-goal-rewrite-command doc r ip
  show-list doc (show-constraints r) = "Cmd_constraints" ∷ Rewrite.show r ∷ []
  show-list doc (show-metas r) = "Cmd_metas" ∷ Rewrite.show r ∷ []
  show-list doc (make-case ip) = "Cmd_make_case" ∷ show-goal-command doc ip

  show : TextDocument.t → t → String
  show = intercalate " " ∘₂ show-list

module AgdaInteraction where
  record t : Set where
    constructor iotcm
    field
      file : TextDocument.t
      command : AgdaCommand.t
  open t public

  show : t → String
  show (iotcm file cmd) =
    let path = TextDocument.file-name file in
    "IOTCM \"" ++ path ++ "\" NonInteractive Direct (" ++ AgdaCommand.show file cmd ++ ")\n"

  from-AgdaCommand : AgdaCommand.t → IO (Maybe t)
  from-AgdaCommand cmd = do
    just e ← TextEditor.active-editor where _ → pure nothing
    doc ← TextEditor.document e
    pure $ just (iotcm doc cmd)

  under-cursor-command : Model → (InteractionPoint.t → AgdaCommand.t) → IO (Maybe t)
  under-cursor-command model cmd = do
    just e ← TextEditor.active-editor where _ → pure nothing
    doc ← TextEditor.document e
    cursor ← TextDocument.offset-at doc <$> TextEditor.cursor-pos e
    model .loaded-files !? TextDocument.file-name doc |> maybe (pure nothing) λ (record { interaction-points = ips }) → do
     ips |> find (λ ip → OffsetRange.contains? (ip .range) cursor) |> maybe (pure nothing) λ ip → do
      pure $ just (iotcm doc (cmd ip))
open AgdaInteraction using (iotcm ; file ; command) public