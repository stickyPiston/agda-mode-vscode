module TEA.Capability where

open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.Utils using (_<$>_ ; _>>_ ; forM ; case_of_)
open import TEA.System
open import Prelude.Sigma
open import Prelude.Maybe hiding (fmap ; pure ; _>>=_ ; _<$>_)
open import Agda.Builtin.Unit
open import Agda.Builtin.Bool
open import Prelude.Function
open import Prelude.String using (String)
open import Agda.Primitive
open import Prelude.Nat

record Router (app-msg self-msg : Set) : Set where field
    send-to-app  : app-msg → IO ⊤
    send-to-self : self-msg → IO ⊤
open Router

module Cmd where
    record t (_ : Set) : Set where field
        action : IO ⊤
    open t public

record Capability (app-msg : Set) : Set₁ where field
    state self-msg : Set ; provision-type : Maybe Set
    provisions : maybe (λ τ → (self-msg → IO ⊤) → τ) ⊤ provision-type
    init : System → Router app-msg self-msg → IO state
    update : System → Router app-msg self-msg → self-msg → state → IO state

postulate Disposable : Set

module EventEmitter where
    postulate t : Set → Set

    postulate new : ∀ {A} → IO (t A)
    {-# COMPILE JS new = _ => cont => cont(new EventEmitter()) #-}

    postulate emit : ∀ {A} → t A → A → IO ⊤
    {-# COMPILE JS emit = _ => emitter => data => cont => { emitter.emit("event", data); cont(a => a["tt"]()) } #-}

    postulate listen : ∀ {A} → t A → (A → IO ⊤) → IO Disposable
    {-# COMPILE JS listen = _ => emitter => listener => cont => {
        const f = data => listener(data)(() => {});
        emitter.on("event", f);
        cont({ disposable() { emitter.off("event", f); } })
    } #-}

module HList where
    open import Prelude.Fin
    open import Prelude.List

    data t : List.t Set → Set₁ where
        [] : t List.[]
        _∷_ : ∀ {A As} → A → t As → t (A List.∷ As)

    _!!_ : ∀ {As} → t As → (i : Fin (List.length As)) → As List.!! i
    (x ∷ _) !! zero = x
    (_ ∷ l) !! suc i = l !! i

open import Prelude.List
module CmdQueue {n} {app-msg : Set} (caps : Vec (Capability app-msg) n) where
    open import Iepje.Internal.JS.Language.MutableReferences renaming (new to new-ref)
    open import Iepje.Internal.Utils using (void)
    open import Prelude.List
    open List hiding (t)
    open import Prelude.Fin
    open Capability using (self-msg)

    data c : Set where
        to-app : app-msg → c
        to-cap : (idx : Fin ∥ caps ∥) → (caps List.!! idx) .self-msg → c

    record t : Set₁ where field
        event : EventEmitter.t c
        currently-evaluating? : Ref Bool
        queue-ref : Ref (List.t c)
        state : HList.t (Vec.map (λ cap → Ref (Capability.state cap)) caps)

    new : IO t
    new = do
        event ← EventEmitter.new
        currently-evaluating? ← new-ref false
        queue-ref ← new-ref []
        pure record
            { event = event
            ; currently-evaluating? = currently-evaluating?
            ; queue-ref = queue-ref
            }

    push-cmd : Ref (List.t c) → c → IO ⊤
    push-cmd queue-ref cmd = void $ modify queue-ref λ queue → queue ++ [ cmd ]

    open import Prelude.Equality

    {-# NON_TERMINATING #-}
    next-cmd : (app-msg → IO ⊤) → t → IO ⊤
    next-cmd app-update t@record { queue-ref = queue-ref ; currently-evaluating? = ce? ; state = state } =
        get queue-ref >>= λ where
            [] → pure tt
            (cmd ∷ new-queue) → do
                set queue-ref new-queue
                set ce? true
                case cmd of λ where
                    (to-app msg) → app-update msg
                    (to-cap idx msg) → do
                        let update = Capability.update (caps List.!! idx)
                        -- σ ← get (state HList.!! idx)
                        let prf = sym (cong Fin (map-length-preserving caps (Ref ∘ Capability.state)))
                        let c-idx = coerce prf idx
                        let s = state HList.!! c-idx
                        {! get s  !}
                set ce? false
                next-cmd app-update t

    setup-listener : (app-msg → IO ⊤) → t → IO Disposable
    setup-listener app-update t@record { event = event ; currently-evaluating? = ce? ; queue-ref = queue-ref } =
        EventEmitter.listen event λ cmd → do
            push-cmd queue-ref cmd
            get ce? >>= λ { true → pure tt ; false → next-cmd app-update t }

    push : t → c → IO ⊤
    push record { event = event } cmd = EventEmitter.emit event cmd

module Runtime where
    open import Iepje.Internal.JS.Language.MutableReferences
    open import Iepje.Internal.Utils using (void)
    open import Prelude.List
    open List using ([] ; _∷_ ; _++_ ; [_] ; ∥_∥)
    open import Prelude.Fin
    open HList
    open Capability

    private variable app-msg : Set

    with-provisions : ∀ {msg} → List.t (Capability msg) → Set → Set
    with-provisions capabilities base =
           map-maybe Capability.provision-type capabilities
        |> List.foldr (λ provision ac → provision → ac) base


    -- {-# NON_TERMINATING #-}
    -- update-cap : ∀ {app-msg} → System → (app-msg → IO ⊤) → (cap : Capability app-msg) → Ref (cap .state) → cap .self-msg → IO ⊤
    -- update-cap sys update-app cap state msg = do
    --     σ ← get state
    --     let router = record { send-to-app = update-app ; send-to-self = update-cap sys update-app cap state }
    --     σ' ← cap .update sys router msg σ
    --     set state σ'
        
    -- update-app : Ref model → (model → msg → model × Cmd.t msg) → 

    Router-type : Capability app-msg → Set
    Router-type {app-msg} cap = Router app-msg (cap .self-msg)

    open import Prelude.Equality

    mk-Routers' : (caps₁ : Vec (Capability app-msg) n) (caps₂ : Vec (Capability app-msg) m)
                → CmdQueue.t (caps₁ ++ caps₂) → HList.t (Vec.map Router-type caps₂)

    mk-Routers : (caps₁ caps₂ : List.t (Capability app-msg)) → CmdQueue.t (caps₁ ++ caps₂) → HList.t (List.map Router-type caps₂)
    mk-Routers caps₁ [] cmd-queue = []
    mk-Routers {app-msg} caps₁ (x ∷ caps₂) cmd-queue rewrite sym (cong CmdQueue.t (List.++-assoc caps₁ [ x ] caps₂)) =
        let router = record { send-to-self = send-to-self' ; send-to-app  = λ msg → push cmd-queue (to-app msg) }
         in router ∷ mk-Routers (caps₁ ++ [ x ]) caps₂ cmd-queue
        where
            open CmdQueue ((caps₁ ++ [ x ]) ++ caps₂)

            valid-index : ∀ xs ys z → suc ∥ xs ∥ ≤ ∥ (xs ++ [ z ]) ++ ys ∥
            valid-index [] ys z = sn≤sm z≤n
            valid-index (_ ∷ xs) ys z = sn≤sm (valid-index xs ys z)

            to-index : ∀ xs ys z → Fin ∥ (xs ++ [ z ]) ++ ys ∥
            to-index xs ys z = weaken-Fin (valid-index xs ys z) (to-Fin ∥ xs ∥)

            indexing-lemma : ∀ xs ys z → z ≡ (((xs ++ [ z ]) ++ ys) List.!! to-index xs ys z)
            indexing-lemma [] ys z = refl
            indexing-lemma (_ ∷ xs) ys z = indexing-lemma xs ys z

            send-to-self' : x .self-msg → IO ⊤
            send-to-self' msg rewrite cong self-msg (indexing-lemma caps₁ caps₂ x) =
                push cmd-queue (to-cap (to-index caps₁ caps₂ x) msg)

    interact :
        ∀ {model msg : Set}
        → System
        → (capabilities : List.t (Capability msg))
        → (init : model)
        → (update : with-provisions capabilities (model → msg → model × List.t (Cmd.t msg)))
        → IO ⊤
    interact {model} {msg} sys capabilities init update = do
        -- 1. Initialise a CmdQueue
        cmd-queue ← CmdQueue.new capabilities
        let routers = mk-Routers [] capabilities cmd-queue
        let base-update = provide-provisions capabilities routers update

        -- 2. Make states for app and capabilities
        app-model-ref ← new init
        CmdQueue.setup-listener capabilities (cmd-queue-app-listener app-model-ref base-update cmd-queue) cmd-queue

        -- 3. Prepare update functions for making `Cmd.t`s
        -- 4. Pass those functions to each of the provision functions
        -- 5. Create base-update function by passing the provisions to the the update
        -- 6. Pass base-update and CmdQueue to setup-listener
        -- 7. Process each capability's init function
        {!   !}
        where
            provide-provisions : (caps : List.t (Capability msg))
              → HList.t (List.map Router-type caps)
              → with-provisions caps (model → msg → model × List.t (Cmd.t msg))
              → model → msg → model × List.t (Cmd.t msg)
            provide-provisions [] [] base = base
            provide-provisions (record { provision-type = nothing } ∷ caps) (_ ∷ routers) base =
                provide-provisions caps routers base
            provide-provisions (record { provision-type = just _ ; provisions = g } ∷ caps) (record { send-to-self = send-to-self } ∷ routers) base =
                provide-provisions caps routers (base (g send-to-self))

            cmd-queue-app-listener : Ref model → (model → msg → model × List.t (Cmd.t msg)) → CmdQueue.t capabilities → msg → IO ⊤
            cmd-queue-app-listener model-ref base-update cmd-queue msg = do
                model ← get model-ref
                let new-model ,, cmds = base-update model msg
                set model-ref new-model
                void $ forM cmds Cmd.action

module Panel where
    open import Prelude.JSON

    data Msg : Set where
        open-panel-cmd : String → Msg

    postulate Panel : Set
    postulate create-webview-panel : vscode-api → IO Panel
    postulate on-message : Panel → (JSON → IO ⊤) → IO ⊤

    record State : Set where field
        panel : Maybe Panel

    capability : ∀ {app-msg} → (JSON → app-msg) → Capability app-msg
    capability {app-msg} webview-msg = record
        { self-msg = Msg
        ; state = State
        ; provision-type = just (String → Cmd.t app-msg)
        ; provisions = λ send-to-self html → record { action = send-to-self (open-panel-cmd html) }
        ; init = λ _ router → do
            pure record { panel = nothing }
        ; update = λ sys router msg state → case msg of λ where
            (open-panel-cmd html) → do
                panel ← create-webview-panel (sys .vscode)
                on-message panel λ json → router .send-to-app (webview-msg json)
                pure record { panel = just panel }
        }