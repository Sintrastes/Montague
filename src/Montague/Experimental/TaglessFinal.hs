
{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies, GADTs, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Montague.Experimental.TaglessFinal where

-- An experimental tagless final implementation of montague semantics.

import GHC.Types
import Data.Data
import Montague.Experimental.LambekType
import Montague.Experimental.Typed (Person(..))

class TermAlg term where
    laml  :: (term b -> term a) -> term (a / b)
    lamr  :: (term a -> term b) -> term (a \\ b)
    appl  :: term (a / b) -> term b -> term a
    appr  :: term (a \\ b) -> term a -> term b
    atom :: Typeable a => a -> term (T a)

class TermAlg term => TermExprAlg term where
    var :: String -> term a

class SemAlg term where
    pred :: String -> [term] -> term
    individual :: Person   -> term
    and  :: term -> term   -> term
    or   :: term -> term   -> term
    all  :: (term -> term) -> term
    some :: (term -> term) -> term
    not  :: term   -> term
    svar  :: String -> term
    the  :: term   -> term
