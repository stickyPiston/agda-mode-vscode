module AgdaMode.Extension where

open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.PrimitiveTypes
open import Iepje.Internal.JS.Language.MutableReferences
open import Iepje.Prelude hiding (Maybe; just; nothing; interact; _>>_) renaming (_++_ to _++ˢ_)
open import Iepje.Internal.Utils using (forM; _>>_; _<$>_)

open import Prelude.Nat using (ℕ ; _-_)
open import Prelude.Sigma
open import Prelude.Vec as Vec
open import Prelude.Maybe
open import Prelude.List
open import Prelude.String

open import Agda.Primitive using (lsuc)

open import AgdaMode.Common.Communication
open import AgdaMode.Common.InteractionResponse
open import Prelude.JSON
import TEA
open import TEA.Cmd
open import TEA.System
open import Vscode.SemanticTokensProvider
open import Vscode.Panel

postulate
    Process : Set

    log : ∀{A : Set} → A → IO null

{-# COMPILE JS log = _ => thing => cont => { console.log(String(thing)); cont(a => a["tt"]()) } #-}

postulate trace : ∀ {A B : Set} → A → B → B
{-# COMPILE JS trace = _ => _ => thing => ret => { console.log(thing); return ret } #-}

postulate spawn : process-api → String → List String → IO Process
{-# COMPILE JS spawn = process => cmd => args => cont => { const p = process.spawn(cmd, args) ; cont(p) } #-}

postulate write : Process → String → IO null
{-# COMPILE JS write = process => chunk => cont => { process.stdin.write(chunk); process.once("drain", () => {}); cont(null) } #-}

postulate read : Process → IO String
{-# COMPILE JS read = proc => cont => cont(proc.stdout.read()) #-}

postulate Buffer : Set

postulate on-data : Process → (Buffer → IO ⊤) → IO ⊤
{-# COMPILE JS on-data = proc => handler => cont => {
    proc.stdout.on("data", data => { handler(data)(() => {}) });
    cont(a => a["tt"]())
} #-}

record Model : Set where field
    panel : Maybe Panel
    proc : Process
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
    agda-stdout-update : Buffer → Msg

    -- Webview messages
    received-webview : WebviewMsg → Msg

open TEA Msg


send-over-stdin-cmd : Process → Cmd Msg
send-over-stdin-cmd proc =
    let path = "/Users/terra/Desktop/code/agda-ffi-tests/Main.lagda.md"
     in mk-Cmd λ dispatch → do
        write proc $ "IOTCM \"" ++ˢ path ++ˢ "\" NonInteractive Direct (Cmd_load \"" ++ˢ path ++ˢ "\" [])\n"
        pure tt

read-stdout-cmd : Process → (Buffer → Msg) → Cmd Msg
read-stdout-cmd proc stdin-msg = mk-Cmd λ dispatch →
    on-data proc λ d → dispatch (stdin-msg d)

postulate buffer-from : String → Buffer
{-# COMPILE JS buffer-from = Buffer.from #-}

init : Process → Model × Cmd Msg
init proc = record
    { panel = nothing
    ; proc = proc
    ; stdout-buffer = ""
    } , read-stdout-cmd proc agda-stdout-update

-- TODO: Legend not exhaustive yet
-- TODO: Make a better api for the legend, maybe something with a Bounded+Enum type class
legend : vscode-api → Legend
legend = mk-Legend ("comment" ∷ "namespace" ∷ "enum" ∷ "type" ∷ "enumMember" ∷ []) ("defaultLibrary" ∷ [])

capabilities : List (Capability Msg)
capabilities =
      command "agda-mode.open-panel" open-webview-msg
    ∷ command "agda-mode.load-file" load-file-msg
    ∷ semantic-tokens-provider legend token-request-msg (language "agda" ∩ scheme "file")
    ∷ []

postulate buffer-toString : Buffer → String
{-# COMPILE JS buffer-toString = buf => buf.toString() #-}

postulate try : ∀ {A : Set} → (⊤ → A) → A
{-# COMPILE JS try = _ => a => {
    try { return a(); } catch (e) { console.log(e); throw e; }
} #-}

to-Cmd : Panel → AgdaResponse → Cmd Msg
to-Cmd panel (DisplayInfo errors _ _ _) = sendMessage panel (show-errors (length errors))

open-panel : Model → System → (Panel → Msg) → (WebviewMsg → Msg) → Cmd Msg
open-panel record { panel = panel } = case panel of λ where
    nothing  → open-panel-cmd
    (just _) → λ _ _ _ → none

update : System → Cmd Msg → Model → Msg → Model × Cmd Msg
update system request-token-msg model msg = trace msg $ case msg of λ where
    load-file-msg → model , batch (send-over-stdin-cmd (Model.proc model) ∷ open-panel model system panel-opened received-webview ∷ [])
    (agda-stdout-update buffer) → try λ _ →
        -- TODO: Agda duplicates the computation of the rhs when pattern matching on a record like this
        let n , lines = split (Model.stdout-buffer model ++ˢ buffer-toString buffer) "\n"
            responses , new-buffer = unsnoc lines
            k , parsed-responses = Vec.map-maybe parse-response responses
         in trace responses $ record model { stdout-buffer = new-buffer } , case (Model.panel model) of λ where
            (just panel) → batch $ map (to-Cmd panel) $ Vec.to-list parsed-responses
            nothing → none

    open-webview-msg → model , open-panel model system panel-opened received-webview
    (panel-opened panel) → record model { panel = just panel } , none

    (token-request-msg return) → model , return [] -- TODO:

    (received-webview wmsg) → model , none

activate : System → IO ⊤
activate sys = do
    proc ← spawn (System.process sys) "agda" [ "--interaction-json" ]
    interact (init proc) capabilities (update sys) sys