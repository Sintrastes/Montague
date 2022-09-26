
module Montague.Experimental.Worlds where

-- | An example "possible worlds" boolean
-- over a linear (future and past) temporal
-- logic.
type TBool = Int -> Bool

-- | "Graded" boolean type -- truth values
-- over possible worlds w.
type GBool w = w -> Bool

-- | The data for a computational representation of Kripke semantics,
-- consisting of the current world, and a relation (represented as a function
-- with multiple return values) representing "related worlds"
data Kripke w = Kripke w (w -> [w])

