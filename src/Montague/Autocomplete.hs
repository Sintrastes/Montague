
module Montague.Autocomplete(
  getAutocomplete
) where

import Montague

-- | Take an annotated term, and return a list of possible atoms 
-- that could be added to the right to produce a new production.
getAutocomplete :: MontagueSemantics a t x -> AnnotatedTerm a t -> [a]
getAutocomplete semantics current = undefined
-- Note: To do this, look to see if the type has a 
-- left-facing arrow at the rightmost position.
-- If so, we can look for all possibilities of the given type.
-- In the future we could even possibly use combinations of words
-- in the search.