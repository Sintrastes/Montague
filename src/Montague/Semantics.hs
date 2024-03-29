
module Montague.Semantics where

import Montague.Types

-- | Typeclass defining an interpretation of the atom type a and
-- base types t in the type x.
data MontagueSemantics a t x = MontagueSemantics {
    typeOfAtom :: a -> MontagueType t,
    parseTerm  :: String -> MontagueTerm a,
    interp     :: AnnotatedTerm a t -> x
}

typeOf :: MontagueSemantics a t x -> Term a -> MontagueType t
typeOf semantics (Atom x) = (typeOfAtom semantics) x
typeOf semantics _ = undefined