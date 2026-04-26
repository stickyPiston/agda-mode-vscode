module AgdaMode.Extension.Goals where

private
  postulate trace : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} → A → B → B
  {-# COMPILE JS trace = _ => _ => _ => _ => a => b => { console.log(a); return b } #-}

module Goal where
  open import Data.Nat hiding (show ; _≤_)
  import Data.Nat as Nat
  open import Data.String hiding (show ; ∥_∥ ; slice)
  import Data.String as String
  open import Data.List hiding (_++_)
  import Data.List as List
  open import Data.Maybe
  open import Function hiding (id)
  open import Data.JSON.Decode hiding (any)
  open import Effect.Monad
  open import Node.Process
  open import Data.IO
  open import Agda.Builtin.Unit
  open import Data.Bool
  open import Data.Product

  open import Vscode.TextEditor

  open MonadPlus ⦃ ... ⦄

  parens : String → String
  parens s = "(" ++ s ++ ")"

  module Position where
    record t : Set where
      constructor mkPosition
      field
        col line pos : ℕ
    open t public

    show : t → String
    show p = intercalate " " $
      "Pn" ∷ "()" ∷ Nat.show (p .pos) ∷ Nat.show (p .line) ∷ Nat.show (p .col) ∷ []

    decode : Decoder t
    decode = ⦇ mkPosition (required "col" nat) (required "line" nat) (required "pos" nat) ⦈

    _≤_ : t → t → 𝔹
    a ≤ b = a .pos Nat.≤ b .pos

  module Interval where
    record t : Set where
      constructor mkInterval
      field
        start end : Position.t
    open t public

    show : t → String
    show i = intercalate " " $
      "Interval" ∷ "()" ∷ map parens (Position.show (i .start) ∷ Position.show (i .end) ∷ [])

    decode : Decoder t
    decode = ⦇ mkInterval (required "start" Position.decode) (required "end" Position.decode) ⦈

    _∈_ : Position.t → t → 𝔹
    p ∈ i = i .start ≤ p ∧ p ≤ i .end
      where open Position

  module Range where
    record t : Set where
      constructor mkRange
      field
        intervals : List Interval.t
    open t public

    show : t → String
    show r = "intervalsToRange () [" ++ intercalate ", " (map Interval.show $ r .intervals) ++ "]"

    decode : Decoder t
    decode = ⦇ mkRange (list Interval.decode) ⦈

    _in-range_ : Position.t → t → 𝔹
    p in-range r = any (p ∈_) (r .intervals)
      where open Interval

  module InteractionPoint where
    record t : Set where
      constructor mkInteractionPoint
      field
        id : ℕ
        range : Range.t
        answer : Maybe String
    open t public

    decoder : Decoder t
    decoder = ⦇ mkInteractionPoint (required "id" nat) (required "range" Range.decode) (pure nothing) ⦈

    response-decoder : Decoder (List t)
    response-decoder = do
      "InteractionPoints" ← required "kind" string where _ → ⊘
      required "interactionPoints" (list decoder)

  give-command : InteractionPoint.t → String → String
  give-command goal s =
    let open InteractionPoint
        interaction-id = Nat.show (goal .id)
        range = "noRange" -- Range.show (goal .range)
    in intercalate " " $
        "Cmd_give" ∷ "WithoutForce" ∷ parens interaction-id ∷ range ∷ String.show s ∷ []

-- Diffs --

-- private module Diff where
--   open import Data.Nat
--   open import Data.Bool
--   open import Data.String
--   open import Data.Maybe
--   open import Data.List hiding (∥_∥ ; map)
--   open import Data.Monoid
--   open import Data.Product
--   open import Function

--   open import Vscode.TextEditor

--   module Interval where
--     record t : Set where
--       constructor interval
--       field start length : Nat
--     open t public
--   open Interval using (interval ; start ; length) public

--   module Replacement where
--     record t : Set where
--       constructor replace
--       field start length replacement-length : Nat
--     open t public

--     _precedes_ : t → t → Bool
--     a precedes b = a .start + a .length < b .start

--     _distantly-precedes_ : t → t → Bool
--     a distantly-precedes b = a .start + a .length + 1 < b .start

--     disjoint? : t → t → Bool
--     disjoint? a b = a precedes b ∨ b precedes a

--     distantly-disjoint? : t → t → Bool
--     distantly-disjoint? a b = a distantly-precedes b ∨ b distantly-precedes a

--     data RelativeOrder : Set where
--       before contained after : RelativeOrder

--     compare : t → Nat → RelativeOrder
--     compare replacement offset =
--       if offset ≤ replacement .start then before
--       else if (replacement .start + replacement .length) ≤ offset then after
--       else contained

--     dual : t → t
--     dual (replace s l n) = replace s n l

--     shift coshift : t → t → Maybe t
--     shiftᵢ coshiftᵢ : t → Interval.t → Maybe Interval.t

--     shift (replace rs₁ l₁ rl₁) r@(replace rs₂ l₂ rl₂) =
--       if rs₂ + l₂ ≤ rs₁ then just r
--       else if rs₁ + l₁ ≤ rs₂ then just (replace (rs₂ + rl₁ - l₁) l₂ rl₂)
--       else nothing
--     shiftᵢ (replace rs l rl) i@(interval is il) =
--       if is + il ≤ rs then just i
--       else if rs + l ≤ is then just (interval (is + rl - l) il)
--       else nothing

--     coshift = shift ∘ dual
--     coshiftᵢ = shiftᵢ ∘ dual

--     private
--       -- Apply r₁ after r₂
--       combine : t → t → t
--       combine r₁@(replace s₁ l₁ n₁) r₂@(replace s₂ l₂ n₂) =
--         if s₁ + l₁ ≤ s₂ then
--           {- We have applied 2, and 1 distantly precedes 2

--                 |---------1---------|    |----2----|
--           -}
--           replace s₁ (s₂ + l₂ - s₁) (n₁ + (s₂ - (s₁ + l₁)) + n₂)
--         else if s₁ ≤ s₂ ∧ s₁ + l₁ < s₂ + n₂ then
--           {- We have applied 2, and 1 straddles on the left of 2.

--                                       |----2----|
--                                  |----1----|
--             new replaced length: |---------|-----|
--                                     /\     /--^--------------\
--                                     n₁     s₂ + n₂ - (s₁ + l₁)
--           -}
--           replace s₁ (s₂ + l₂ - s₁) (s₂ + n₂ - (s₁ + l₁) + n₁)
--         else if s₁ ≤ s₂ ∧ s₁ + l₁ ≥ s₂ + n₂ then
--           {- We have applied 2, and 1 fully contains 2.

--                                     |-----2-----|
--                             |-------------1---------------|
--             original range: |-------|-----------|---------|
--                              /--^--\     /\       /--^----------------\
--                              s₂ - s₁     l₂       (s₁ + l₁) - (s₂ + n₂)
--           -}
--           replace s₁ ((s₂ - s₁) + l₂ + ((s₁ + l₁) - (s₂ + n₂))) n₁
--         else if s₁ < s₂ + n₂ ∧ s₂ + n₂ < s₁ + l₁ then
--           {- We have applied 2, and 1 straddles on the right of 2.

--                                       |----2----|
--                                            |----1----|
--           -}
--           replace s₂ (l₂ + (s₁ + l₁) - (s₂ + n₂)) (s₁ + n₁ - s₂)
--         else if s₁ < s₂ + n₂ ∧ s₂ + n₂ ≥ s₁ + l₁ then
--           {- We have applied 2, and 2 fully contains 1

--                                       |---------2---------|
--                                            |----1----|
--           -}
--           replace s₂ l₂ ((s₁ - s₂) + n₁ + ((s₂ + n₂) - (s₁ + l₁)))
--         else
--           {- We have applied 2, and 2 distantly precedes 1

--                 |---------2---------|    |----1----|
--           -}
--           replace s₂ (l₂ + (s₁ - (s₂ + n₂)) + l₁) (s₁ + n₁ - s₂)

--     instance
--       Replacement-Semigroup : Semigroup t
--       Replacement-Semigroup = record { _<>_ = combine }

--     module _ where
--       open import Agda.Builtin.Equality

--       open Semigroup {{ ... }}

--       _ : replace 0 0 10 <> replace 9 1 0 ≡ replace 0 10 19
--       _ = refl

--   open Replacement hiding (t) public

--   open Semigroup {{ ... }}

--   module Diff where
--     -- Diff is a monotonically increasing sequence on source locations of distantly disjoint replacements
--     t : Set
--     t = List Replacement.t

--     to-Replacement : t → Replacement.t
--     to-Replacement [] = replace 0 0 0
--     to-Replacement (x ∷ xs) = mconcat⁺ (x |: xs)

--     data InsertionPoint : Set where
--       left right : InsertionPoint
--       middle : t → Replacement.t → t → InsertionPoint

--     find-insertion-point : (Replacement.t → Bool) → t → Replacement.t → InsertionPoint
--     find-insertion-point p Δ r = Δ
--         -- Because Diff is monotonically increasing and the predicate is also monotonic, the partition will
--         -- return a split in which both parts will also adhere to the invariants of Diff
--         |> partition p
--         |> case_of λ where
--           ([] , _) → left
--           (_ , []) → right
--           (l , s ∷ r)  → middle l s r

--     left-add-and-merge : Replacement.t → t → t
--     left-add-and-merge r [] = [ r ]
--     left-add-and-merge r@(replace s l n) (t ∷ Δ) =
--       if r Replacement.distantly-precedes t
--         then r ∷ t ∷ Δ
--         else left-add-and-merge (r <> t) Δ

--     postulate error : {A : Set} → A
--     {-# COMPILE JS error = _ => { throw new Error("error") } #-}

--     insert : Replacement.t → t → t
--     insert rp Δ = case find-insertion-point not-precedes Δ rp of λ where
--         left → left-add-and-merge rp Δ
--         (middle l s r) → l <> left-add-and-merge (from-just $ coshift (to-Replacement l) rp) (s ∷ r)
--         -- from-just because `coshift (to-Replacement Δ) r` can never return nothing
--         right → case coshift (to-Replacement Δ) rp of λ where
--           (just x) → snoc x Δ
--           nothing → trace rp $ trace (to-Replacement Δ) Δ
--       where
--         not-precedes : Replacement.t → Bool
--         not-precedes s = s .start + s .replacement-length ≤ rp .start

--     map : t → Interval.t → Maybe Interval.t
--     map Δ i =
--       let r = replace (i .start) (i .length) (i .length)
--           not-precedes s = s .start + s .length < r .start
--           pos = find-insertion-point not-precedes Δ r
--        in case pos of λ where
--             left → just i -- Unaffected by the diff
--             (middle l s r) → shiftᵢ (to-Replacement l) i
--             right → shiftᵢ (to-Replacement Δ) i

--   handle-change : TextDocumentContentChangeEvent.t → Diff.t → Diff.t
--   handle-change change Δ =
--     let open TextDocumentContentChangeEvent
--         replace = record { start = change .range-offset ; length = change .range-length ; replacement-length = ∥ change .text ∥ }
--      in Diff.insert replace Δ

-- open Diff public
