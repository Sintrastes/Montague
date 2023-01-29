
module Montague.Experimental.PregroupGrammar where

-- | A free pregroup is defined as a sequences of powers of x^l, or powers of x^r.
type FreePregroup a = [FreePregroupPower a]

-- | Formal powers of elements of a, either powers of x^l, or powers of x^r
data FreePregroupPower a = 
    LeftPow  Int a
  | RightPow Int a

-- | Just some really basic categories to try this out with.
data ToyCats = N | S | NP