module AgdaMode.Extension.Position where

open import Data.Nat hiding (show)
import Data.Nat as Nat
open import Data.Int hiding (_+_)
open import Data.Bool
open import Data.Maybe
open import Data.String hiding (∥_∥ ; show ; _==_)
import Data.String as String
open import Data.List hiding (_++_)
open import Function

open import Vscode.TextEditor

_∈[_⋯_] : Nat → Nat → Nat → Bool
n ∈[ lo ⋯ hi ] = lo ≤ n ∧ n ≤ hi

module OffsetRange where
  record t : Set where
    constructor offset-range
    field
      start length : Nat
  open t public

  end : t → Nat
  end r = r .start + r .length - 1

  shift : Int → t → t
  shift (pos n) r = record r { start = r .start + n }
  shift (negsuc n) r = record r { start = r .start - suc n }

  contains? : t → Nat → Bool
  contains? r o = o ∈[ r .start ⋯ r .start + r .length ]

  show : t → String
  show (offset-range start length) = "offset-range " ++ Nat.show start ++ " " ++ Nat.show length

  equals? : t → t → Bool
  equals? (offset-range s₁ l₁) (offset-range s₂ l₂) = s₁ Nat.== s₂ ∧ l₁ Nat.== l₂

  open import Vscode.Common

  to-vsc-range : TextDocument.t → t → Range.t
  to-vsc-range doc (offset-range start length) =
    let start-pos = TextDocument.position-at doc start in
    let end-pos = TextDocument.position-at doc (start + length) in
    Range.new start-pos end-pos

open OffsetRange using (offset-range ; start ; length) public

module Change where
  record t : Set where
    constructor replace_with-length_
    field
      range : OffsetRange.t
      by : Nat
  open t public

  show : t → String
  show (replace range with-length by) = "replace " ++ OffsetRange.show range ++ " with-length " ++ Nat.show by

  new-range : t → OffsetRange.t
  new-range (replace range with-length by) = offset-range (range .start) by

  shift : Int → t → t
  shift n change = record change { range = OffsetRange.shift n (change .range) } 

  influences? : OffsetRange.t → t → Bool
  influences? r₁ (replace r₂ with-length _) = r₂ .length |> λ where
    -- Insertions are allowed right up to the range
    0 → (OffsetRange.start r₁ < OffsetRange.start r₂ ∧ OffsetRange.start r₂ ≤ OffsetRange.end r₁)
      ∨ (OffsetRange.start r₁ < OffsetRange.end r₂ ∧ OffsetRange.end r₂ ≤ OffsetRange.end r₁)

    -- If we're dealing with a replacement, then replacing the first character will also influence the range
    _ → (OffsetRange.start r₁ ≤ OffsetRange.start r₂ ∧ OffsetRange.start r₂ ≤ OffsetRange.end r₁)
      ∨ (OffsetRange.start r₁ ≤ OffsetRange.end r₂ ∧ OffsetRange.end r₂ ≤ OffsetRange.end r₁)

  open TextDocumentContentChangeEvent hiding (t)

  from-TextDocumentContentChangeEvent : TextDocumentContentChangeEvent.t → t
  from-TextDocumentContentChangeEvent change =
    replace offset-range (change .range-offset) (change .range-length) with-length ∥ split (change .text) "" ∥
open Change using (replace_with-length_ ; range ; by) public
