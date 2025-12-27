module TEA (msg : Set) where

open import Agda.Builtin.String
open import Prelude.List
open import Agda.Builtin.Unit
open import Prelude.Sigma
open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.MutableReferences
open import Agda.Primitive
open import Prelude.Maybe
open import Agda.Builtin.Bool
open import Iepje.Internal.Utils using (case_of_ ; _>>_ ; forM ; _$_)
open import Prelude.JSON

open import Vscode.SemanticTokensProvider
open import TEA.Cmd
open import TEA.System
postulate Disposable : Set

data Capability (msg : Set) : Set where
    command : String → msg → Capability msg
    semantic-tokens-provider : (vscode-api → Legend) → ((List SemanticToken → Cmd msg) → msg) → LanguageFilter → Capability msg

capability-requirement : Capability msg → Set
capability-requirement (command _ _) = ⊤
capability-requirement (semantic-tokens-provider _ _ _) = EventEmitter

provided-commands-type : List (Capability msg) → Set → Set
provided-commands-type capabilities base-type =
    let command-types = concat-map (λ where
            (command _ _) → []
            (semantic-tokens-provider _ _ _) → [ Cmd msg ]) capabilities
     in foldr (λ a b → a → b) base-type command-types

data HList {ℓ} : List (Set ℓ) → Set (lsuc ℓ) where
    []  : HList []
    _∷_ : ∀ {a as} → a → HList as → HList (a ∷ as)

provide-capability-commands :
    ∀ {model}
    → (capabilities : List (Capability msg))
    → provided-commands-type capabilities (model → msg → model × Cmd msg)
    → IO (HList (map capability-requirement capabilities) × (model → msg → model × Cmd msg))
provide-capability-commands [] base-update = pure ([] , base-update)
provide-capability-commands (command _ _ ∷ capabilities) base-update = do
    smaller ← provide-capability-commands capabilities base-update
    pure ((tt ∷ Σ.proj₁ smaller) , Σ.proj₂ smaller)
provide-capability-commands (semantic-tokens-provider _ _ _ ∷ capabilities) base-update = do
    on-change-emitter ← new-event-emitter
    smaller ← provide-capability-commands capabilities (base-update (mk-Cmd λ  _ → fire on-change-emitter))
    pure ((on-change-emitter ∷ Σ.proj₁ smaller) , Σ.proj₂ smaller)

postulate Document CancellationToken : Set

-- TODO: Handle cancellations
-- TODO: resolve takes the output of SemanticTokensBuilder.build()
postulate register-semantic-tokens-provider : vscode-api → JSON → EventEmitter → (Document → CancellationToken → (return : List SemanticToken → IO ⊤) → IO ⊤) → Legend → IO Disposable
{-# COMPILE JS register-semantic-tokens-provider = vscode => selector => onChangeEmitter => provider => legend => vscode.languages.registerDocumentSemanticTokensProvider(
    selector,
    {
        onDidChangeSemanticTokens: onChangeEmitter.event,
        provideDocumentSemanticTokens: (document, token) => new Promise((resolve, reject) => { provider(document)(token)(resolve)(() => {}) }),
    },
    legend
) #-}

postulate queue-ref : Set → Set

postulate new-queue-ref : ∀ {A : Set} → IO (queue-ref A)
{-# COMPILE JS new-queue-ref = _ => cont => cont({ val: [] }) #-}

postulate enqueue : ∀ {A : Set} → queue-ref A → A → IO ⊤
{-# COMPILE JS enqueue = _ => queueRef => a => cont => { queueRef.val.push(a); cont(a => a["tt"]()) } #-}

postulate dequeue : ∀ {A : Set} → queue-ref A → IO (Maybe A)
{-# COMPILE JS dequeue = _ => queueRef => cont => 
    cont(queueRef.val.length === 0 ? undefined : queueRef.val.shift()) #-}

postulate empty? : ∀ {A : Set} → queue-ref A → IO Bool
{-# COMPILE JS empty? = _ => queueRef => cont => { cont(queueRef.val.length === 0) } #-}

postulate register-command : vscode-api → String → IO ⊤ → IO Disposable
{-# COMPILE JS register-command = vscode => name => action => cont => { cont(vscode.commands.registerCommand(name, () => { action(_ => {}) })) } #-}

postulate push-subscription : Disposable → extension-context → IO ⊤
{-# COMPILE JS push-subscription = cmd => context => cont => { context.subscriptions.push(cmd); cont(a => a["tt"]()); } #-}

-- This is a separate function because of a bug in JS backend
-- We cannot put update-and-process-commands in a where block under interact
-- because it will overwrite the code for lambda blocks, and break itself.
{-# NON_TERMINATING #-}
update-and-process-commands : (model msg : Set) → (update : model → msg → model × Cmd msg) → queue-ref ((msg → IO ⊤) → IO ⊤) → Ref model → msg → IO ⊤
update-and-process-commands model msgₜ update cmd-queue model-ref msg = do
    current-model ← get model-ref
    case (update current-model msg) of λ where
        (new-model , record { actions = actions }) → do
            forM actions $ enqueue cmd-queue
            set model-ref new-model
            dequeue cmd-queue >>= λ where
                (just new-cmd) → new-cmd (update-and-process-commands model msgₜ update cmd-queue model-ref)
                nothing → pure tt

-- Execute the TEA application
-- beware: gonna be ugly agda code
interact :
    ∀ {model}
    → (init : model × Cmd msg)
    → (capabilities : List (Capability msg))
    → (update : provided-commands-type capabilities (model → msg → model × Cmd msg))
    → System
    → IO ⊤
interact {model} (init-model , init-cmds) capabilities update record { vscode = vscode ; context = context } = do
    model-ref ← new init-model
    cmd-queue ← new-queue-ref { (msg → IO ⊤) → IO ⊤ }

    requirements , update-with-capabilities ← provide-capability-commands capabilities update

    forM (Cmd.actions init-cmds) λ action →
        action (update-and-process-commands model msg update-with-capabilities cmd-queue model-ref)

    register-capabilities model-ref cmd-queue update-with-capabilities capabilities requirements
    where
        register-capability : 
            ∀ {model} → Ref model → queue-ref ((msg → IO ⊤) → IO ⊤)
            → (model → msg → model × Cmd msg)
            → (capability : Capability msg) → capability-requirement capability
            → IO Disposable
        register-capability {model} model-ref cmd-queue update (command name on-trigger-msg) tt =
            register-command vscode name (update-and-process-commands model msg update cmd-queue model-ref on-trigger-msg)
        register-capability {model} model-ref cmd-queue update (semantic-tokens-provider legend on-request-msg selector) onChangeEmitter =
            let provider = λ doc token return → update-and-process-commands model msg update cmd-queue model-ref (on-request-msg λ tokens → mk-Cmd λ _ → return tokens)
                in register-semantic-tokens-provider vscode (encode-language-filter selector) onChangeEmitter provider (legend vscode)

        register-capabilities :
            ∀ {model} → Ref model → queue-ref ((msg → IO ⊤) → IO ⊤)
            → (model → msg → model × Cmd msg)
            → (capabilities : List (Capability msg)) → HList (map capability-requirement capabilities)
            → IO ⊤
        register-capabilities model-ref cmd-queue update [] [] = pure tt
        register-capabilities model-ref cmd-queue update (cap ∷ capabilities) (req ∷ requirements) = do
            disposable ← register-capability model-ref cmd-queue update cap req
            push-subscription disposable context
