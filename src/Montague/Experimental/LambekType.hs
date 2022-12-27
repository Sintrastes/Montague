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