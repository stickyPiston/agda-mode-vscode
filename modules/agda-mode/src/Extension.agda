module Extension where

open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.PrimitiveTypes
open import Iepje.Internal.JS.Language.MutableReferences
open import Iepje.Prelude hiding (Maybe; just; nothing; interact; _>>_)
open import Iepje.Internal.Utils hiding (_×_)
open import Agda.Builtin.List
open import Agda.Builtin.Maybe
open import Communication

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
    postMessage : Panel → IO null
    onMessage : Panel → extension-context → (JSON → IO ⊤) → IO ⊤

{-# COMPILE JS register-command = vscode => name => action => cont => { cont(vscode.commands.registerCommand(name, () => { action(_ => {}) })) } #-}
{-# COMPILE JS createWebviewPanel = vscode => cont => cont(vscode.window.createWebviewPanel("window", "window", vscode.ViewColumn.One, { enableScripts: true }))  #-}
{-# COMPILE JS push-subscription = cmd => context => cont => { context.subscriptions.push(cmd); cont(null); } #-}
{-# COMPILE JS log = _ => thing => cont => { console.log(String(thing)); cont(null) } #-}
{-# COMPILE JS extensionUri = context => context.extensionUri #-}
{-# COMPILE JS joinPath = vscode => parts => vscode.Uri.joinPath(...parts) #-}
{-# COMPILE JS toWebviewUri = panel => url => panel.webview.asWebviewUri(url) #-}
{-# COMPILE JS setHtml = html => panel => cont => { panel.webview.html = html; cont(null) } #-}
{-# COMPILE JS postMessage = panel => cont => { panel.webview.postMessage(null); cont(null) } #-}
{-# COMPILE JS onMessage = panel => ctx => action => cont => { panel.webview.onDidReceiveMessage(msg => action(msg)(() => {}), undefined, ctx.subscriptions); cont(a => a["tt"]()); } #-}

postulate trace : ∀ {A B : Set} → A → B → B
{-# COMPILE JS trace = _ => _ => thing => ret => { console.log(thing); return ret } #-}

sendMessage : Ref (Maybe Panel) → IO null
sendMessage panel =
    get panel >>= λ where
        (just p) → postMessage p
        nothing → log "No panel set"

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

postulate on-data : Process → (String → IO ⊤) → IO ⊤
{-# COMPILE JS on-data = proc => handler => cont => {
    proc.stdout.on("data", data => { handler(data.toString())(() => {}) });
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

postulate new-queue-ref : ∀ {A} → IO (queue-ref A)
{-# COMPILE JS new-queue-ref = _ => cont => cont({ val: [] }) #-}

postulate enqueue : ∀ {A} → queue-ref A → A → IO ⊤
{-# COMPILE JS enqueue = _ => queueRef => a => cont => { queueRef.val.push(a); cont(a => a["tt"]()) } #-}

postulate dequeue : ∀ {A} → queue-ref A → IO (Maybe A)
{-# COMPILE JS dequeue = _ => queueRef => cont => 
    cont(b => queueRef.val.length === 0 ? b["nothing"]() : b["just"](queueRef.val.shift())) #-}

postulate empty? : ∀ {A} → queue-ref A → IO boolean
{-# COMPILE JS empty? = _ => queueRef => cont => { cont(queueRef.val.length === 0) } #-}

record Cmd (msg : Set) : Set where field
    actions : List ((dispatch : msg → IO ⊤) → IO ⊤)

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

data Msg : Set where
    -- Vscode messages
    load-file-msg : Msg
    open-webview-msg : Msg
    panel-opened : Panel → Msg
    
    -- Agda messages
    agda-update : JSON → Msg

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
        ++ toWebviewUri panel (joinPath vscode (extensionUri context ∷ "out" ∷ "jAgda.Webview.mjs" ∷ []))
        ++ "></script></body></html>") panel
    onMessage panel context λ json → case (decode json) of λ where
        (just wmsg) → dispatch (webview-msg wmsg)
        nothing     → do
            _ ← log "Could not parse message"
            pure tt
    pure tt

send-over-stdin-cmd : Process → Cmd Msg
send-over-stdin-cmd proc =
    let path = "/Users/terra/Desktop/code/agda-mode-agda/modules/agda-mode/src/Extension.agda"
     in mk-Cmd λ dispatch → do
        write proc $ "IOTCM \"" ++ path ++ "\" NonInteractive Direct (Cmd_load \"" ++ path ++ "\" [])\n"
        pure tt

postulate parse-json : String → Maybe JSON
{-# COMPILE JS parse-json = input => {
    try {
        const json = JSON.parse(input);
        console.log(json);
    } catch (_e) {
        console.log(input, _e);
    }
    return a => a["nothing"]();
} #-}

read-stdout-cmd : Process → (JSON → Msg) → Cmd Msg
read-stdout-cmd proc stdin-msg = mk-Cmd λ dispatch →
    on-data proc λ d →
        case parse-json d of λ where
            (just json) → trace json $ dispatch (stdin-msg json)
            nothing     → pure tt

init : Process → Model × Cmd Msg
init proc = record { panel = nothing ; proc = proc } , read-stdout-cmd proc agda-update

commands : List (vsc-cmd Msg)
commands =
      command "agda-mode.open-panel" open-webview-msg
    ∷ command "agda-mode.load-file" load-file-msg
    ∷ []

update : System → Model → Msg → Model × Cmd Msg
update record { process = process ; vscode = vscode ; context = context } model = λ where
    load-file-msg → model , send-over-stdin-cmd (Model.proc model)
    (agda-update json) → trace json $ model , none

    open-webview-msg → model , open-panel-cmd vscode context panel-opened received-webview
    (panel-opened panel) → record model { panel = just panel } , none

    (received-webview wmsg) →
        trace (case wmsg of λ where
            a → "Received a!"
            b → "Received b!") (model , none)

activate : System → IO ⊤
activate sys = do
    proc ← spawn (System.process sys) "agda" [ "--interaction-json" ]
    interact (init proc) commands (update sys) sys