module AgdaMode.Extension where

open import Iepje.Internal.JS.Language.IO
open import Iepje.Prelude hiding (Maybe; just; nothing; interact; _>>_)
open import Iepje.Internal.Utils using (_>>_)

open import Prelude.Sigma
open import Prelude.Vec using (unsnoc ; map-maybe ; to-list)
open import Prelude.List using (map ; [_])
open import Prelude.Maybe
open import Prelude.String

open import AgdaMode.Common.Communication
open import AgdaMode.Common.InteractionResponse
import TEA
open import TEA.Cmd hiding (new)
open import TEA.System
open System
open import TEA.Capability
open import Vscode.SemanticTokensProvider
open import Vscode.Command
open import Vscode.Panel
open import Vscode.StdioProcess

-- Debug functions

postulate trace : ∀ {A B : Set} → A → B → B
{-# COMPILE JS trace = _ => _ => thing => ret => { console.log(thing); return ret } #-}

postulate try : ∀ {A : Set} → (⊤ → A) → A
{-# COMPILE JS try = _ => a => {
    try { return a(); } catch (e) { console.log(e); throw e; }
} #-}

-- Actual app

record Model : Set where field
    panel : Maybe Panel
    stdout-buffer : String

-- NO_POSITIVITY_CHECK needed to get around the non-positive position of Msg in token-request-msg
{-# NO_POSITIVITY_CHECK #-}
data Msg : Set where
    -- Vscode messages
    load-file-msg : Msg
    open-webview-msg : Msg
    panel-opened : Panel → Msg
    token-request-msg : (List SemanticToken → Cmd Msg) → Msg
    
    -- Agda messages
    agda-stdout-update : Buffer.t → Msg

    -- Webview messages
    received-webview : WebviewMsg → Msg

open TEA Msg

init : Model × Cmd Msg
init = record
    { panel = nothing
    ; stdout-buffer = ""
    } , none

-- TODO: Legend not exhaustive yet
-- TODO: Make a better api for the legend, maybe something with a Bounded+Enum type class
legend : vscode-api → Legend.t
legend = Legend.new ("comment" ∷ "namespace" ∷ "enum" ∷ "type" ∷ "enumMember" ∷ []) ("defaultLibrary" ∷ [])

capabilities : List (Capability Msg)
capabilities =
      command "agda-mode.open-panel" open-webview-msg
    ∷ command "agda-mode.load-file" load-file-msg
    ∷ semantic-tokens-provider legend token-request-msg (language "agda" ∩ scheme "file")
    ∷ stdio-process "agda" [ "--interaction-json" ] agda-stdout-update
    ∷ []

to-Cmd : Panel → AgdaResponse → Cmd Msg
to-Cmd panel (DisplayInfo errors _ _ _) = sendMessage panel (show-errors (length errors))

open-panel : Model → System → (Panel → Msg) → (WebviewMsg → Msg) → Cmd Msg
open-panel record { panel = panel } = case panel of λ where
    nothing  → open-panel-cmd
    (just _) → λ _ _ _ → none

update : Cmd Msg → (String → Cmd Msg) → System → Model → Msg → Model × Cmd Msg
update request-token-cmd send-over-stdin-cmd system model msg = trace msg $ case msg of λ where
    load-file-msg →
        let path = "/Users/terra/Desktop/code/agda-ffi-tests/Main.lagda.md"
         in model , batch
            ( send-over-stdin-cmd ("IOTCM \"" ++ path ++ "\" NonInteractive Direct (Cmd_load \"" ++ path ++ "\" [])\n")
            ∷ open-panel model system panel-opened received-webview
            ∷ [])
    (agda-stdout-update buffer) → try λ _ →
        -- TODO: Agda duplicates the computation of the rhs when pattern matching on a record like this
        let n , lines = split (Model.stdout-buffer model ++ Buffer.toString buffer) "\n"
            responses , new-buffer = unsnoc lines
            k , parsed-responses = map-maybe parse-response responses
         in trace responses $ record model { stdout-buffer = new-buffer } , case (Model.panel model) of λ where
            (just panel) → batch $ map (to-Cmd panel) $ to-list parsed-responses
            nothing → none

    open-webview-msg → model , open-panel model system panel-opened received-webview
    (panel-opened panel) → record model { panel = just panel } , none

    (token-request-msg return) → model , return [] -- TODO:

    (received-webview wmsg) → model , none

activate : System → IO ⊤
activate = interact init capabilities update