
module Montague.Semantics where

import Control.Monad.Tree
import Data.PartialOrd
import Montague.Types

-- | Typeclass defining an interpretation of the atom type a and
-- base types t in the type x.
class (Eq x, PartialOrd t) => MontagueSemantics a t x | a -> t, a -> x where
    typeOf    :: Term a t -> MontagueType t
    parseTerm :: String -> MontagueTerm a t
    interp    :: AnnotatedTerm a t -> x