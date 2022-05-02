{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Montague.Autocomplete(
  getAutocomplete
) where

import Montague
import Data.Function
import Data.Functor
import Control.Monad.Tree
import Control.Monad

-- | Take a term, and return a list of possible atoms 
-- that could be added to the right to produce a new production.
getAutocomplete :: forall a t. (Eq t, Enum a, Bounded a) => MontagueSemantics a t x -> MontagueTerm a t -> [a]
getAutocomplete semantics current = let t = typeOf semantics <$> current in join $ bfs $
  -- look to see if the type has a 
  -- left-facing arrow at the rightmost position.
  join t <&> \case
    LeftArrow x y ->
      -- If so, we can look for all possibilities of the given type.
      -- In the future we could even possibly use combinations of words
      -- in the search.
      filter (\x -> typeOf semantics (Atom x) == pure y) ([minBound..maxBound] :: [a])
    _ -> 
      -- Otherwise there will be no matches
      []