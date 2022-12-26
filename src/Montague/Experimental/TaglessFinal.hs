
{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies, GADTs, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

module Montague.Experimental.TaglessFinal where

-- An experimental tagless final implementation of montague semantics.

import GHC.Types
import Data.Data

class TermAlg _Ω term where
    lam  :: (term _Ω a -> term _Ω b) -> term _Ω (a -> b)
    app  :: term _Ω (a -> b) -> term _Ω a -> term _Ω b
    atom :: Typeable a => a -> term _Ω a
    and  :: term _Ω _Ω -> term _Ω _Ω -> term _Ω _Ω
    or   :: term _Ω _Ω -> term _Ω _Ω -> term _Ω _Ω
    all  :: (term _Ω a    -> term _Ω _Ω) -> term _Ω _Ω
    some :: (term _Ω _Ω -> term _Ω _Ω) -> term _Ω _Ω
    will :: term _Ω _Ω -> term _Ω _Ω
    willAlways :: term _Ω _Ω -> term _Ω _Ω

class TermAlg _Ω term => TermExprAlg _Ω term where
    var :: String -> term _Ω a