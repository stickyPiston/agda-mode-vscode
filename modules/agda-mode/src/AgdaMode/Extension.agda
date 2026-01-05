module AgdaMode.Extension where

open import Iepje.Internal.JS.Language.IO using (IO)
open import Iepje.Prelude hiding (Maybe; just; nothing; interact; _>>_ ; for ; empty)
open import Iepje.Internal.Utils using (_>>_)

open import Prelude.Sigma
open import Prelude.Vec using (unsnoc ; to-list ; for)
open import Prelude.List using (map ; [_])
open import Prelude.Maybe hiding (map-maybe)
open import Prelude.String
open import Prelude.JSON
open import Prelude.JSON.Decode hiding (_<$>_ ; pure)
open import Prelude.Map

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
open import Vscode.Window
open import AgdaMode.Extension.Highlighting

-- Debug functions

postulate trace : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} → A → B → B
{-# COMPILE JS trace = _ => _ => _ => _ => thing => val => { console.log(thing) ; return val } #-}

postulate try : ∀ {A : Set} → (⊤ → A) → A
{-# COMPILE JS try = _ => a => {
    try { return a(); } catch (e) { console.log(e); throw e; }
} #-}

-- Actual app

record Model : Set where field
    panel : Maybe Panel
    stdout-buffer : String
    current-doc : Maybe TextDocument.t
    loaded-files : StringMap (List Token)
open Model

-- NO_POSITIVITY_CHECK needed to get around the non-positive position of Msg in token-request-msg
{-# NO_POSITIVITY_CHECK #-}
data Msg : Set where
    -- Vscode messages
    load-file-msg : Msg
    open-webview-msg : Msg
    panel-opened : Panel → Msg
    token-request-msg : TextDocument.t → (List SemanticToken.t → Cmd Msg) → Msg
    new-active-editor : Maybe TextEditor.t → Msg
    
    -- Agda messages
    agda-stdout-update : Buffer.t → Msg

    -- Webview messages
    received-webview : WebviewMsg → Msg

open TEA Msg

init : Model × Cmd Msg
init = record
    { panel = nothing
    ; stdout-buffer = ""
    ; current-doc = nothing
    ; loaded-files = empty
    } , none

capabilities : List (Capability Msg)
capabilities =
      command "agda-mode.open-panel" open-webview-msg
    ∷ command "agda-mode.load-file" load-file-msg
    ∷ semantic-tokens-provider legend token-request-msg (language "agda" ∩ scheme "file")
    ∷ stdio-process "agda" [ "--interaction-json" ] agda-stdout-update
    ∷ on-did-change-active-text-editor new-active-editor
    ∷ []

kind-decoder : Decoder String
kind-decoder = required "kind" string

update : Cmd Msg → (String → Cmd Msg) → System → Model → Msg → Model × Cmd Msg
update request-token-cmd send-over-stdin-cmd system model msg = trace msg $ case msg of λ where
    load-file-msg → model , from-Maybe none do
        path ← TextDocument.file-name <$> model .current-doc
        pure $ send-over-stdin-cmd ("IOTCM \"" ++ path ++ "\" NonInteractive Direct (Cmd_load \"" ++ path ++ "\" [])\n")

    (agda-stdout-update buffer) → try λ _ →
        -- TODO: Agda duplicates the computation of the rhs when pattern matching on a record like this
        let n , lines = split (model .stdout-buffer ++ Buffer.toString buffer) "\n"
            responses , new-buffer = unsnoc lines
            new-model = record model { stdout-buffer = new-buffer }
         in from-Maybe (new-model , none) do
                parsed-responses ← traverse-Vec parse-response responses
                highlighting-response ← find-Vec (λ r → primStringEquality "HighlightingInfo" <$> kind-decoder r or-else false) parsed-responses
                tokens ← highlighting-info-decoder highlighting-response
                -- TODO: Eventually we should probably take better account of which command this update is a response to
                path ← TextDocument.file-name <$> model .current-doc
                pure (record new-model { loaded-files = model .loaded-files [ path ]:= tokens } , request-token-cmd)

    -- open-webview-msg → model , open-panel model system panel-opened received-webview
    -- (panel-opened panel) → record model { panel = just panel } , none

    -- TODO: We need to return an empty list of tokens because vscode will wait for reply.
    --       If one isn't given then, it will just ignore subsequent replies. Ideally, we would use 
    --       reject with a busy message.
    (token-request-msg doc return-tokens-cmd) → from-Maybe (model , return-tokens-cmd []) do
        _ ← trace (model .loaded-files) $ pure tt
        tokens ← model .loaded-files !? TextDocument.file-name doc
        let highlighting-tokens = make-highlighting-tokens (system .vscode) doc tokens
        pure $ model , return-tokens-cmd highlighting-tokens

    (new-active-editor nothing) → record model { current-doc = nothing } , none
    (new-active-editor (just editor)) → record model { current-doc = just $ TextEditor.document editor } , none

    -- (received-webview wmsg) → model , none
    _ → model , none

activate : System → IO ⊤
activate = interact init capabilities update