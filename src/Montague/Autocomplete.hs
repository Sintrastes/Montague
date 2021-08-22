
module Montague.Autocomplete(
  getAutocomplete
) where

import Montague

-- | Take an annotated term, and return a list of possible atoms 
-- that could be added to the right to produce a new production.
getAutocomplete :: MontagueSemantics a t x => AnnotatedTerm a t -> [a]
getAutocomplete current = undefined