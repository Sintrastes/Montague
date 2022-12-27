{-# LANGUAGE TypeOperators, DataKinds, GADTs #-}

module Montague.Experimental.LambekType where
import Data.Kind

data LambekType where 
    T       :: Type -> LambekType
    L       :: LambekType -> LambekType -> LambekType
    R       :: LambekType -> LambekType -> LambekType
    Extract :: LambekType -> LambekType -> LambekType
    Scoped  :: LambekType -> LambekType -> LambekType
    Conj    :: LambekType -> LambekType -> LambekType
    Disj    :: LambekType -> LambekType -> LambekType

type (∧) = Conj
type (∨) = Disj
type (/) = L
type (\\) = R
type (↑) = Extract
type (⇑) = Scoped