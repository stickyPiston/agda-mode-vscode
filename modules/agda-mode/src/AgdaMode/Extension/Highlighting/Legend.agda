module AgdaMode.Extension.Highlighting.Legend where

open import Data.Maybe
open import Data.Bool
open import Data.String hiding (_==_ ; show)
open import Data.List
open import Data.IO
open import Function
open import Effect.Monad
open Monad {{ ... }}

open import Vscode.TextEditor

open import AgdaMode.Extension.Highlighting.Decode

module TokenType where
  data t : Set where
    namespace enum-member operator string function comment : t
    keyword number type property parameter struct variable' : t

  from-PrimaryAspect : PrimaryAspect.t → t
  from-PrimaryAspect aspect = aspect |> λ where
    PrimaryAspect.comment → comment
    PrimaryAspect.keyword → keyword
    PrimaryAspect.string → string
    PrimaryAspect.number → number
    PrimaryAspect.hole → variable'
    PrimaryAspect.symbol → operator
    PrimaryAspect.primitive-type → type
    PrimaryAspect.pragma → keyword
    PrimaryAspect.background → comment
    PrimaryAspect.markup → comment
    (PrimaryAspect.name _ true) → operator
    (PrimaryAspect.name nothing false) → variable'
    (PrimaryAspect.name (just kind) false) → kind |> λ where
      NameKind.bound → parameter ; NameKind.generalisable → variable'
      NameKind.inductive-constructor → enum-member ; NameKind.coinductive-constructor → enum-member
      NameKind.datatype → type ; NameKind.field' → property ; NameKind.function → function
      NameKind.module' → namespace ; NameKind.postulate' → function ; NameKind.primitive' → variable'
      NameKind.record' → struct ; NameKind.argument → variable' ; NameKind.macro' → variable'

  show : t → String
  show namespace = "namespace"
  show enum-member = "enumMember"
  show operator = "operator"
  show string = "string"
  show function = "function"
  show comment = "comment"
  show keyword = "keyword"
  show number = "number"
  show type = "type"
  show property = "property"
  show parameter = "parameter"
  show struct = "struct"
  show variable' = "variable"

module Modifier where
  data t : Set where
    default-library : t
  

module HighlightDecoration where
  data t : Set where 
    unsolved termination-problem coverage-problem : t
    positivity-problem missing-definition fatal-warning unused : t
    not-definitional confluence-problem : t

  enum : List t
  enum = 
    unsolved ∷ termination-problem ∷ coverage-problem ∷
    positivity-problem ∷ missing-definition ∷ fatal-warning ∷ unused ∷
    not-definitional ∷ confluence-problem ∷ []

  _==_ : t → t → Bool
  unsolved == unsolved = true
  termination-problem == termination-problem = true
  coverage-problem == coverage-problem = true
  positivity-problem == positivity-problem = true
  missing-definition == missing-definition = true
  fatal-warning == fatal-warning = true
  unused == unused = true
  not-definitional == not-definitional = true
  confluence-problem == confluence-problem = true
  _ == _ = false

  from-SecondaryAspect : SecondaryAspect.t → t
  from-SecondaryAspect aspect = aspect |> λ where
    SecondaryAspect.error → fatal-warning
    SecondaryAspect.error-warning → fatal-warning
    SecondaryAspect.dotted-pattern → fatal-warning
    SecondaryAspect.unsolved-meta → unsolved
    SecondaryAspect.unsolved-constraint → unsolved
    SecondaryAspect.termination-problem → termination-problem
    SecondaryAspect.positivity-problem → positivity-problem
    SecondaryAspect.dead-code → unused
    SecondaryAspect.shadowing-in-telescope → unused
    SecondaryAspect.coverage-problem → coverage-problem
    SecondaryAspect.type-checks → unused
    SecondaryAspect.missing-definition → missing-definition
    SecondaryAspect.instance-problem → fatal-warning
    SecondaryAspect.cosmetic-problem → fatal-warning
    SecondaryAspect.catchall-clause → not-definitional
    SecondaryAspect.confluence-problem → confluence-problem

  private
    bg-decoration : String → IO DecorationType.t
    bg-decoration colour = DecorationType.Options.new
      <&> DecorationType.Options.set-background-colour colour
      >>= DecorationType.create

  new : t → IO DecorationType.t
  new unsolved = bg-decoration "#ffff00"
  new termination-problem = bg-decoration "#ffa07a"
  new coverage-problem = bg-decoration "#f5deb3"
  new positivity-problem = bg-decoration "#cd853f"
  new missing-definition = bg-decoration "#ffa500"
  new fatal-warning = bg-decoration "#f08080"
  new unused = bg-decoration "#bebebe"
  new not-definitional = bg-decoration "#f5f5f5"
  new confluence-problem = bg-decoration "#ffc0cb"

module HighlightDecorationMap where
  t : Set
  t = HighlightDecoration.t → DecorationType.t

  open HighlightDecoration hiding (t)

  init : IO t
  init = do
    ud ← new unsolved ; tpd ← new termination-problem ; cpd ← new coverage-problem
    ppd ← new positivity-problem ; mdd ← new missing-definition ; fwd ← new fatal-warning
    und ← new unused ; ndd ← new not-definitional ; cfd ← new confluence-problem
    pure λ where
      unsolved → ud ; termination-problem → tpd ; coverage-problem → cpd
      positivity-problem → ppd ; missing-definition → mdd ; fatal-warning → fwd
      unused → und ; not-definitional → ndd ; confluence-problem → cfd