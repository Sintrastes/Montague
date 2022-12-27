
{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies, GADTs, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Montague.Experimental.TaglessFinal where

-- An experimental tagless final implementation of montague semantics.

import GHC.Types
import Data.Data
import Montague.Experimental.LambekType

class TermAlg term where
    laml  :: (term b -> term a) -> term (a / b)
    lamr  :: (term a -> term b) -> term (a \\ b)
    appl  :: term (a / b) -> term b -> term a
    appr  :: term (a \\ b) -> term a -> term b
    atom :: Typeable a => a -> term (T a)

class TermAlg term => TermExprAlg term where
    var :: String -> term a