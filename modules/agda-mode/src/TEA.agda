module TEA (msg : Set) where

open import Agda.Builtin.String
open import Prelude.List
open import Agda.Builtin.Unit
open import Prelude.Sigma
open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.MutableReferences
open import Agda.Primitive
open import Prelude.Maybe
open import Iepje.Internal.Utils using (case_of_ ; _>>_ ; forM ; _$_)

open import TEA.Cmd hiding (new)
open import TEA.System
open System
open import TEA.Capability
open Capability

provided-commands-type : List (Capability msg) → Set → Set
provided-commands-type capabilities base-type =
    let command-types = map-maybe (λ c → Σ.proj₁ <$> c .provided-type) capabilities
     in foldr (λ a b → a → b) base-type command-types

data HList {ℓ} : List (Set ℓ) → Set (lsuc ℓ) where
    []  : HList []
    _∷_ : ∀ {a as} → a → HList as → HList (a ∷ as)

provide-capability-commands :
    ∀ {model} → System
    → (capabilities : List (Capability msg))
    → provided-commands-type capabilities (System → model → msg → model × Cmd msg)
    → IO (HList (map requirement-type capabilities) × (model → msg → model × Cmd msg))
provide-capability-commands sys [] base-update = pure ([] , base-update sys)
provide-capability-commands sys (record { new-requirement = new-requirement ; provided-type = nothing } ∷ capabilities) base-update = do
    requirement ← new-requirement sys
    smaller ← provide-capability-commands sys capabilities base-update
    pure ((requirement ∷ Σ.proj₁ smaller) , Σ.proj₂ smaller)
provide-capability-commands sys (record
    { requirement-type = requirement-type ; new-requirement = new-requirement
    ; provided-type = just (provided-type , create-provided-type) ; register = register } ∷ capabilities) base-update = do
    requirement ← new-requirement sys
    smaller ← provide-capability-commands sys capabilities (base-update (create-provided-type requirement))
    pure ((requirement ∷ Σ.proj₁ smaller) , Σ.proj₂ smaller)

postulate queue-ref : Set → Set

postulate new-queue-ref : ∀ {A : Set} → IO (queue-ref A)
{-# COMPILE JS new-queue-ref = _ => cont => cont({ val: [] }) #-}

postulate enqueue : ∀ {A : Set} → queue-ref A → A → IO ⊤
{-# COMPILE JS enqueue = _ => queueRef => a => cont => { queueRef.val.push(a); cont(a => a["tt"]()) } #-}

postulate dequeue : ∀ {A : Set} → queue-ref A → IO (Maybe A)
{-# COMPILE JS dequeue = _ => queueRef => cont => 
    cont(queueRef.val.length === 0 ? undefined : queueRef.val.shift()) #-}

postulate push-subscription : extension-context → Disposable → IO ⊤
{-# COMPILE JS push-subscription = context => cmd => cont => { context.subscriptions.push(cmd); cont(a => a["tt"]()); } #-}

-- This is a separate function because of a bug in JS backend
-- We cannot put update-and-process-commands in a where block under interact
-- because it will overwrite the code for lambda blocks, and break itself.
{-# NON_TERMINATING #-}
update-and-process-commands : {model : Set} → (update : model → msg → model × Cmd msg) → queue-ref ((msg → IO ⊤) → IO ⊤) → Ref model → msg → IO ⊤
update-and-process-commands update cmd-queue model-ref msg = do
    current-model ← get model-ref
    case (update current-model msg) of λ where
        (new-model , record { actions = actions }) → do
            forM actions $ enqueue cmd-queue
            set model-ref new-model
            dequeue cmd-queue >>= λ where
                (just new-cmd) → new-cmd (update-and-process-commands update cmd-queue model-ref)
                nothing → pure tt

-- Execute the TEA application
-- beware: gonna be ugly agda code
interact :
    ∀ {model}
    → (init : model × Cmd msg)
    → (capabilities : List (Capability msg))
    → (update : provided-commands-type capabilities (System → model → msg → model × Cmd msg))
    → System
    → IO ⊤
interact {model} (init-model , init-cmds) capabilities update system = do
    model-ref ← new init-model
    cmd-queue ← new-queue-ref { (msg → IO ⊤) → IO ⊤ }

    requirements , update-with-capabilities ← provide-capability-commands system capabilities update

    forM (Cmd.actions init-cmds) λ action →
        action (update-and-process-commands update-with-capabilities cmd-queue model-ref)

    register-capabilities model-ref cmd-queue update-with-capabilities capabilities requirements
    where
        register-capabilities :
              Ref model → queue-ref ((msg → IO ⊤) → IO ⊤)
            → (model → msg → model × Cmd msg)
            → (capabilities : List (Capability msg)) → HList (map requirement-type capabilities)
            → IO ⊤
        register-capabilities model-ref cmd-queue update [] [] = pure tt
        register-capabilities model-ref cmd-queue update (record { register = register } ∷ capabilities) (req ∷ requirements) =
            let update' = update-and-process-commands update cmd-queue model-ref
             in register system req update' >>= λ where
                (just disposable) → push-subscription (system .context) disposable
                nothing           → pure tt
