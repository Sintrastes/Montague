
module Montague.Semantics where

import Montague.Types

-- | Typeclass defining an interpretation of the atom type a and
-- base types t in the type x.
data MontagueSemantics a t x = MontagueSemantics {
    typeOf    :: Term a t -> MontagueType t,
    parseTerm :: String -> MontagueTerm a t,
    interp    :: AnnotatedTerm a t -> x
}