
module Montague.Experimental.Worlds where

-- | An example "possible worlds" boolean
-- over a linear (future and past) temporal
-- logic.
type TBool = Int -> Bool

-- | "Graded" boolean type -- truth values
-- over possible worlds w.
type GBool w = w -> Bool