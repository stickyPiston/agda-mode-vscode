module TEA.Cmd where

open import Prelude.List using (List ; [_] ; concat-map ; [])
open import Iepje.Internal.JS.Language.IO
open import Agda.Builtin.Unit

record Cmd (msg : Set) : Set where field
    actions : List ((dispatch : msg → IO ⊤) → IO ⊤)

batch : {msg : Set} → List (Cmd msg) → Cmd msg
batch cmds = record { actions = concat-map (λ cmd → Cmd.actions cmd) cmds }

none : ∀ {msg} → Cmd msg
none = record { actions = [] }

new : ∀ {msg} → ((msg → IO ⊤) → IO ⊤) → Cmd msg
new cmd = record { actions = [ cmd ] }
