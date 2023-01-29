{-# LANGUAGE TypeOperators, DataKinds, GADTs, LambdaCase #-}

module Montague.Experimental.LambekType where
import Data.Kind

data NounPhraseType = Of | To | From | On | For | By

-- | Type of terms in the lambek calculus.
data LambekType where 
    -- | Generic constructor for injecting a Haskell type
    -- into a Lambek type. Used for sentences.
    T       :: Type -> LambekType
    NP      :: Maybe NounPhraseType -> Type -> LambekType
    -- | Constructor for a polar interrogative.
    Sy      :: Type -> LambekType
    -- | Constructor for a Wh-interrogative
    Sw      :: Type -> Type -> LambekType
    -- | Specific constructor for injecting a type of individuals
    -- (together with a "boolean type") as a noun.
    N       :: Type -> Type -> LambekType
    -- | Left arrow. Consumes an arugment on the right.
    L       :: LambekType -> LambekType -> LambekType
    -- | Right arrow. Consumes and argument on the left.
    R       :: LambekType -> LambekType -> LambekType
    -- | Extraction operator. Used to account for unbounded dependencies.
    Extract :: LambekType -> LambekType -> LambekType
    -- | Scoping operator. Used to account for the grammar of quantifiers like every and some.
    Scoped  :: LambekType -> LambekType -> LambekType
    -- | Conjunction operator on types. Used for coordnation.
    Conj    :: LambekType -> LambekType -> LambekType
    -- | Disjunction operator on types. Used for coordnation.
    Disj    :: LambekType -> LambekType -> LambekType

-- Typealiases to better fit with the terminology in Carpenter.
type S o  = T o

instance Show LambekType where
  show = \case
    T x         -> "" -- TODO: show $ typeRep x
    R x y       -> show x ++ "\\" ++ show y
    L x y       -> show x ++ "/"  ++ show y
    Conj x y    -> show x ++ "∧"  ++ show y
    Disj x y    -> show x ++ "∨"  ++ show y
    Extract x y -> show x ++ "↑"  ++ show y
    Scoped x y  -> show x ++ "⇑"  ++ show y

type (∧) = Conj
type (∨) = Disj
type (/) = L
type (\\) = R
type (↑) = Extract
type (⇑) = Scoped