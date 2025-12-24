module AgdaMode.Extension where

open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.PrimitiveTypes
open import Iepje.Internal.JS.Language.MutableReferences
open import Iepje.Prelude hiding (Maybe; just; nothing; interact; _>>_)
open import Iepje.Internal.Utils using (forM_; _>>_)

open import Prelude.Nat using (ℕ ; _-_)
open import Prelude.Sigma
open import Prelude.Vec as Vec
open import Prelude.Maybe

open import Agda.Builtin.List

open import AgdaMode.Common.Communication
open import AgdaMode.Common.InteractionResponse
open import AgdaMode.Common.JSON

the : (A : Set) → A → A
the _ t = t

postulate
    Command : Set
    extension-context vscode-api process-api : Set
    Panel : Set
    Process : Set
    text-document uri : Set

    register-command : vscode-api → string → IO ⊤ → IO Command
    createWebviewPanel : vscode-api → IO Panel
    setHtml : string → Panel → IO null
    push-subscription : Command → extension-context → IO null
    extensionUri : extension-context → string
    joinPath : vscode-api → List string → string
    toWebviewUri : Panel → string → string
    log : ∀{A : Set} → A → IO null
    postMessage : Panel → JSON → IO ⊤
    onMessage : Panel → extension-context → (JSON → IO ⊤) → IO ⊤

{-# COMPILE JS register-command = vscode => name => action => cont => { cont(vscode.commands.registerCommand(name, () => { action(_ => {}) })) } #-}
{-# COMPILE JS createWebviewPanel = vscode => cont => cont(vscode.window.createWebviewPanel("window", "window", vscode.ViewColumn.One, { enableScripts: true }))  #-}
{-# COMPILE JS push-subscription = cmd => context => cont => { context.subscriptions.push(cmd); cont(null); } #-}
{-# COMPILE JS log = _ => thing => cont => { console.log(String(thing)); cont(a => a["tt"]()) } #-}
{-# COMPILE JS extensionUri = context => context.extensionUri #-}
{-# COMPILE JS joinPath = vscode => parts => vscode.Uri.joinPath(...parts) #-}
{-# COMPILE JS toWebviewUri = panel => url => panel.webview.asWebviewUri(url) #-}
{-# COMPILE JS setHtml = html => panel => cont => { panel.webview.html = html; cont(null) } #-}
{-# COMPILE JS postMessage = panel => json => cont => { panel.webview.postMessage(json); cont(null) } #-}
{-# COMPILE JS onMessage = panel => ctx => action => cont => { panel.webview.onDidReceiveMessage(msg => action(msg)(() => {}), undefined, ctx.subscriptions); cont(a => a["tt"]()); } #-}

postulate trace : ∀ {A B : Set} → A → B → B
{-# COMPILE JS trace = _ => _ => thing => ret => { console.log(thing); return ret } #-}


record System : Set where
    constructor system
    field
        process : process-api
        vscode  : vscode-api
        context : extension-context

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

-- This should prolly be IO
postulate current-text-documents : vscode-api → List text-document
{-# COMPILE JS current-text-documents = vscode => vscode.workspace.textDocuments #-}

postulate get-document-uri : text-document → uri
{-# COMPILE JS get-document-uri = doc => doc.uri #-}

postulate uri-path uri-scheme : uri → string
{-# COMPILE JS uri-path = uri => uri.path #-}
{-# COMPILE JS uri-scheme = uri => uri.scheme #-}

pattern [_] x = x ∷ []

data vsc-cmd (msg : Set) : Set where
    command : String → msg → vsc-cmd msg

postulate queue-ref : Set → Set

postulate new-queue-ref : ∀ {A : Set} → IO (queue-ref A)
{-# COMPILE JS new-queue-ref = _ => cont => cont({ val: [] }) #-}

postulate enqueue : ∀ {A : Set} → queue-ref A → A → IO ⊤
{-# COMPILE JS enqueue = _ => queueRef => a => cont => { queueRef.val.push(a); cont(a => a["tt"]()) } #-}

postulate dequeue : ∀ {A : Set} → queue-ref A → IO (Maybe A)
{-# COMPILE JS dequeue = _ => queueRef => cont => 
    cont(queueRef.val.length === 0 ? undefined : queueRef.val.shift()) #-}

postulate empty? : ∀ {A : Set} → queue-ref A → IO boolean
{-# COMPILE JS empty? = _ => queueRef => cont => { cont(queueRef.val.length === 0) } #-}

record Cmd (msg : Set) : Set where field
    actions : List ((dispatch : msg → IO ⊤) → IO ⊤)

postulate flat-map : {A B : Set} → (A → List B) → List A → List B
{-# COMPILE JS flat-map = _ => _ => f => l => l.flatMap(f) #-}

batch : {msg : Set} → List (Cmd msg) → Cmd msg
batch cmds = record { actions = flat-map (λ cmd → Cmd.actions cmd) cmds }

-- This is a separate function because of a bug in JS backend
-- We cannot put update-and-process-commands in a where block under interact
-- because it will overwrite the code for lambda blocks, and break itself.
{-# NON_TERMINATING #-}
update-and-process-commands : (model msg : Set) → (update : model → msg → model × Cmd msg) → queue-ref ((msg → IO ⊤) → IO ⊤) → Ref model → msg → IO ⊤
update-and-process-commands model msgₜ update cmd-queue model-ref msg = do
    current-model ← get model-ref
    case (update current-model msg) of λ where
        (new-model , record { actions = actions }) → do
            forM_ actions $ enqueue cmd-queue
            set model-ref new-model
            dequeue cmd-queue >>= λ where
                (just new-cmd) → new-cmd (update-and-process-commands model msgₜ update cmd-queue model-ref)
                nothing → pure tt

-- Execute the TEA application
-- beware: gonna be ugly agda code
interact :
    ∀ {model msg : Set}
    → (init : model × Cmd msg)
    → (commands : List (vsc-cmd msg))
    → (update : model → msg → model × Cmd msg)
    → System
    → IO ⊤
interact {model = modelₜ} {msg = msgₜ} (init-model , init-cmds) commands update record { vscode = vscode ; context = context } = do
    model ← new init-model
    cmd-queue ← new-queue-ref { (msgₜ → IO ⊤) → IO ⊤ }

    forM_ (Cmd.actions init-cmds) λ action →
        action (update-and-process-commands modelₜ msgₜ update cmd-queue model)

    -- Register commands
    forM_ commands λ where
        (command name msg) → do
            registered-cmd ← register-command vscode name (update-and-process-commands modelₜ msgₜ update cmd-queue model msg)
            push-subscription registered-cmd context
            pure tt
    
record Model : Set where field
    panel : Maybe Panel
    proc : Process
    stdout-buffer : String

data Msg : Set where
    -- Vscode messages
    load-file-msg : Msg
    open-webview-msg : Msg
    panel-opened : Panel → Msg
    
    -- Agda messages
    agda-stdout-update : Buffer → Msg

    -- Webview messages
    received-webview : WebviewMsg → Msg

none : ∀ {msg} → Cmd msg
none = record { actions = [] }

mk-Cmd : ∀ {msg} → ((msg → IO ⊤) → IO ⊤) → Cmd msg
mk-Cmd cmd = record { actions = [ cmd ] }

open-panel-cmd : vscode-api → extension-context → (Panel → Msg) → (WebviewMsg → Msg) → Cmd Msg
open-panel-cmd vscode context panel-msg webview-msg = mk-Cmd λ dispatch → do
    panel ← createWebviewPanel vscode
    dispatch (panel-msg panel)
    setHtml ("<html><body><main></main><script type=\"module\" src="
        ++ toWebviewUri panel (joinPath vscode (extensionUri context ∷ "out" ∷ "jAgda.AgdaMode.Webview.mjs" ∷ []))
        ++ "></script></body></html>") panel
    onMessage panel context λ json → case (decode json) of λ where
        (just wmsg) → dispatch (webview-msg wmsg)
        nothing     → do
            _ ← log "Could not parse message"
            pure tt
    pure tt

send-over-stdin-cmd : Process → Cmd Msg
send-over-stdin-cmd proc =
    let path = "/Users/terra/Desktop/code/agda-ffi-tests/Main.lagda.md"
     in mk-Cmd λ dispatch → do
        write proc $ "IOTCM \"" ++ path ++ "\" NonInteractive Direct (Cmd_load \"" ++ path ++ "\" [])\n"
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

commands : List (vsc-cmd Msg)
commands =
      command "agda-mode.open-panel" open-webview-msg
    ∷ command "agda-mode.load-file" load-file-msg
    ∷ []

postulate _==ᵇ_ : Buffer → Buffer → Bool
{-# COMPILE JS _==ᵇ_ = b1 => b2 => Buffer.compare(b1, b2) === 0 #-}

postulate buffer-concat : Buffer → Buffer → Buffer
{-# COMPILE JS buffer-concat = b1 => b2 => Buffer.concat([b1, b2]) #-}

postulate buffer-toString : Buffer → String
{-# COMPILE JS buffer-toString = buf => buf.toString() #-}

postulate try : ∀ {A : Set} → (⊤ → A) → A
{-# COMPILE JS try = _ => a => {
    try { return a(); } catch (e) { console.log(e); throw e; }
} #-}

postulate split : String → String → Σ[ n ∈ ℕ ] Vec.Vec String (suc n)
{-# COMPILE JS split = input => on => {
    /* Array.prototype.split always returns at least one split even if the input string is empty */
    const splits = input.split(on); 
    return [BigInt(splits.length), splits];
} #-}

-- TODO: Make n and m Fin k
postulate slice : ∀ {A : Set} {k} → Vec.Vec A k → (n : ℕ) → (m : ℕ) → Vec.Vec A (m - n)
{-# COMPILE JS slice = _ => _ => l => n => m => l.slice(Number(n), Number(m)) #-}

vec-init : ∀ {A : Set} {n} → Vec.Vec A (suc n) → Vec.Vec A n
vec-init {n = n} as = slice as 0 n

postulate last : ∀ {A : Set} {n : ℕ} → Vec.Vec A (suc n) → A
{-# COMPILE JS last = _ => _ => l => l[l.length - 1] #-}

unsnoc : ∀ {A : Set} {n : ℕ} → Vec.Vec A (suc n) → (Vec.Vec A n × A)
unsnoc xs = vec-init xs , last xs

sendMessage : ∀ {msg} {A} ⦃ c : Cloneable A ⦄ → Panel → A → Cmd msg
sendMessage panel m = mk-Cmd λ _ → postMessage panel (encode m)

to-Cmd : Panel → AgdaResponse → Cmd Msg
to-Cmd panel (DisplayInfo errors _ _ _) = sendMessage panel (show-errors (length errors))

open-panel : Model → vscode-api → extension-context → (Panel → Msg) → (WebviewMsg → Msg) → Cmd Msg
open-panel record { panel = panel } = case panel of λ where
    nothing  → open-panel-cmd
    (just _) → λ _ _ _ _ → none

update : System → Model → Msg → Model × Cmd Msg
update record { process = process ; vscode = vscode ; context = context } model msg = trace msg $ case msg of λ where
    load-file-msg → model , batch (send-over-stdin-cmd (Model.proc model) ∷ open-panel model vscode context panel-opened received-webview ∷ [])
    (agda-stdout-update buffer) → try λ _ →
        -- TODO: Agda duplicates the computation of the rhs when pattern matching on a record like this
        let n , lines = split (Model.stdout-buffer model ++ buffer-toString buffer) "\n"
            responses , new-buffer = unsnoc lines
            k , parsed-responses = Vec.map-maybe parse-response responses
         in trace responses $ record model { stdout-buffer = new-buffer } , case (Model.panel model) of λ where
            (just panel) → batch $ map (to-Cmd panel) $ Vec.to-list parsed-responses
            nothing → none

    open-webview-msg → model , open-panel model vscode context panel-opened received-webview
    (panel-opened panel) → record model { panel = just panel } , none

    (received-webview wmsg) → model , none

activate : System → IO ⊤
activate sys = do
    proc ← spawn (System.process sys) "agda" [ "--interaction-json" ]
    interact (init proc) commands (update sys) sys