module Prelude.List where

-- open import Agda.Builtin.List public
open import Iepje.Internal.JS.Language.IO
open import Agda.Primitive
open import Prelude.Nat
open import Prelude.Fin

private variable
    ℓ ℓ₁ ℓ₂ : Level
    A : Set ℓ₁
    B : Set ℓ₂

module List where
    import Agda.Builtin.List as L
    open L using ([] ; _∷_) public

    pattern [_] x = x ∷ []

    t : Set ℓ → Set ℓ
    t = L.List

    map : (A → B) → t A → t B
    map f [] = []
    map f (x ∷ xs) = f x ∷  map f xs

    _++_ : t A → t A → t A
    [] ++ bs = bs
    (x ∷ as) ++ bs = x ∷ (as ++ bs)

    foldr : (A → B → B) → B → t A → B
    foldr f b [] = b
    foldr f b (a ∷ as) = f a (foldr f b as)

    concat : t (t A) → t A
    concat = foldr _++_ []

    concat-map : (A → t B) → t A → t B
    concat-map f as = concat (map f as)

    foldrM : (A → B → IO B) → B → t A → IO B
    foldrM f b [] = pure b
    foldrM f b (a ∷ as) = foldrM f b as >>= f a

    length : t A → ℕ
    length [] = 0
    length (_ ∷ xs) = suc (length xs)

    ∥_∥ : t A → ℕ
    ∥_∥ = length

    _!!_ : (l : t A) → Fin (length l) → A
    (x ∷ _) !! zero = x
    (_ ∷ l) !! suc i = l !! i

    slice : (l : t A) → Fin (suc (length l)) → t A
    slice [] zero = []
    slice (x ∷ _) zero = []
    slice (x ∷ l) (suc i) = x ∷ slice l i

    open import Prelude.Equality

    ++-assoc : (as bs cs : t A) → (as ++ bs) ++ cs ≡ as ++ (bs ++ cs)
    ++-assoc [] bs cs = refl
    ++-assoc (x ∷ as) bs cs = cong (x ∷_) (++-assoc as bs cs)

    map-length-preserving : {A B : Set ℓ} → (as : t A) (f : A → B) → ∥ map f as ∥ ≡ ∥ as ∥ 
    map-length-preserving [] f = refl
    map-length-preserving (a ∷ as) f = cong suc (map-length-preserving as f)

    apply-map-index : {A B : Set ℓ} → (as : t A) (f : A → B) (i : Fin ∥ as ∥) → map f as !! i ≡ f (as !! i)
    apply-map-index as f i = ?

module List⁺ where
    record t (A : Set ℓ) : Set ℓ where
        constructor _:|_
        field
            head : A
            tail : List.t A

    length : t A → ℕ
    length (_ :| as) = suc (List.length as)