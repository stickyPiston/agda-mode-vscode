module AgdaMode.Extension.Highlighting.Decode where

open import Data.Maybe
open import Data.Bool
open import Data.List
import Data.List as List
open import Data.Nat
open import Data.Product
open import Data.String
import Data.String as String
open import Data.Maybe
open import Data.Maybe.Effectful
open import Data.JSON
open import Data.JSON.Decode hiding (string)
import Data.JSON.Decode as Decode
open import Function

open import Effect.Monad
open MonadPlus {{ ... }}

open import AgdaMode.Extension.Position

module NameKind where
  data t : Set where
    bound generalisable inductive-constructor coinductive-constructor : t
    datatype field' function module' postulate' primitive' record' argument macro' : t

  private
    string-decoder : Decoder t
    string-decoder = Decode.string >>= λ where
      "bound" → succeed bound ; "generalizable" → succeed generalisable ; "inductiveconstructor" → succeed inductive-constructor
      "coinductiveconstructor" → succeed coinductive-constructor ; "datatype" → succeed datatype ; "field" → succeed field'
      "function" → succeed function ; "module" → succeed module' ; "postulate" → succeed postulate' ; "record" → succeed record'
      "argument" → succeed argument ; "macro" → succeed macro' ; _ → ⊘

  decoder : Decoder (Maybe t)
  decoder = list Decode.any <&> (asum ∘ map string-decoder)

module PrimaryAspect where
  data t : Set where
    comment keyword string number hole symbol primitive-type pragma background markup : t
    name : Maybe NameKind.t → Bool → t

  private
    string-decoder : Decoder t
    string-decoder = Decode.string >>= λ where
      "comment" → succeed comment ; "keyword" → succeed keyword ; "string" → succeed string
      "number" → succeed number ; "hole" → succeed hole ; "symbol" → succeed symbol
      "primitivetype" → succeed primitive-type ; "pragma" → succeed pragma 
      "background" → succeed background ; "markup" → succeed markup ; _ → ⊘

    operator-decoder : Decoder Bool
    operator-decoder = list Decode.string <&> List.any (String._== "operator")
 
  decoder : Decoder (Maybe t)
  decoder = do
    things ← list Decode.any
    succeed $ asum (map string-decoder things) <|> (| name (NameKind.decoder (j-array things)) (operator-decoder (j-array things)) |)

module SecondaryAspect where
  data t : Set where
    error error-warning dotted-pattern unsolved-meta unsolved-constraint : t
    termination-problem positivity-problem dead-code shadowing-in-telescope : t
    coverage-problem type-checks missing-definition instance-problem : t
    cosmetic-problem catchall-clause confluence-problem : t

  private
    string-decoder : Decoder t
    string-decoder = Decode.string >>= λ where
      "error" → succeed error ; "errorwarning" → succeed error-warning ; "dottedpattern" → succeed dotted-pattern
      "unsolvedmeta" → succeed unsolved-meta ; "unsolvedconstraint" → succeed unsolved-constraint
      "terminationproblem" → succeed termination-problem ; "positivityproblem" → succeed positivity-problem
      "deadcode" → succeed dead-code ; "shadowingintelescope" → succeed shadowing-in-telescope
      "coverageproblem" → succeed coverage-problem ; "typechecks" → succeed type-checks
      "missingdefinition" → succeed missing-definition ; "instanceproblem" → succeed instance-problem
      "cosmeticproblem" → succeed cosmetic-problem ; "catchallclause" → succeed catchall-clause
      "confluenceproblem" → succeed confluence-problem ; _ → ⊘

  decoder : Decoder (List t)
  decoder = list Decode.any <&> map-Maybe string-decoder  

module DefinitionSite where
  record t : Set where
    constructor mk-DefinitionSite
    field
      filepath : String
      position : Nat
  open t public

  decoder : Decoder t
  decoder = (| mk-DefinitionSite (required "filepath" Decode.string) (required "position" nat) |)
open DefinitionSite using (filepath ; position) public

module Token where
  record t : Set where
    constructor mk-Token
    field
      primary : Maybe PrimaryAspect.t
      secondary : List SecondaryAspect.t -- Officially a set
      note : String
      definition-site : Maybe DefinitionSite.t
      token-based : Bool
      range : OffsetRange.t
  open t public

  private
    range-decoder : Decoder OffsetRange.t
    range-decoder = do
      start ← list nat |> index 0 |> fmap (_- 1)
      end ← list nat |> index 1 |> fmap (_- 1)
      pure $ offset-range start (end - start)

  decoder : Decoder t
  decoder = (| mk-Token
    (required "atoms" PrimaryAspect.decoder)
    (required "atoms" SecondaryAspect.decoder)
    (required "note" Decode.string)
    (optional-null "definitionSite" DefinitionSite.decoder)
    (required "tokenBased" Decode.string <&> ("TokenBased" String.==_))
    (required "range" range-decoder) |)
open Token using (primary ; secondary ; note ; definition-site ; token-based ; range) public

highlighting-info-decoder : Decoder (List Token.t × Bool)
highlighting-info-decoder = required "kind" Decode.string >>= λ where
  "HighlightingInfo" → required "info" (| required "payload" (list Token.decoder) , required "remove" bool |)
  _ → ⊘
