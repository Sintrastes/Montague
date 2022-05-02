{-# LANGUAGE LambdaCase, ScopedTypeVariables, RankNTypes, DataKinds, GADTs, KindSignatures #-}

module Montague (
  module Montague.Types,
  module Montague.Semantics,
  getAllParses,
  getParse,
  getParseFromLexicon,
  getAllParsesFromLexicon
) where

import Prelude hiding ((>=),(<=))
import Control.Monad.Tree
import Control.Monad
import Data.Maybe
import Data.List
import Montague.Types
import Montague.Semantics
import Montague.Lexicon
import Data.PartialOrd hiding (nub, (==), elem)
import Data.Proxy
import Data.Char

-------- Helper functions ---------
split :: Char -> String -> [String]
split c s = case rest of
    []     -> [chunk]
    _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

toTree xs = Node () (fmap Leaf xs)

-- | Create an annotated term from a lexicon and a string
annotate :: MontagueSemantics a t x -> Proxy a -> String -> NonDet [AnnotatedTerm a t]
annotate semantics _ xs = do
    lexes <- mapM (parseTerm semantics) $
        -- Convert everything into lowercase for consistancy
        fmap (fmap toLower) $
        -- Split into words
        split ' ' $
        -- Ignore non-period punctuation.
        filter (\x -> not $ x `elem` ",;") xs
    types <- mapM (typeOf semantics) lexes
    return $ zipWith Annotated lexes types

-- | View pattern to use to split a string of tokens into the pattern
--   xs ++ ys ++ zs.
viewSubstrings :: [a] -> [([a], [a], [a])]
viewSubstrings xs = fmap (`splitTwoAt` xs) [0..l - 2]
    where l = length xs
          splitTwoAt n xs =
              let (start, ys) = splitAt n xs in
              let (middle,end) = splitAt 2 ys in
                    (start,middle,end)

-- | Reduce a list of terms by applying adjacent terms according to their types
reduce :: PartialOrd t => MontagueSemantics a t x -> [AnnotatedTerm a t] -> NonDet x
-- If the list is already reduced, just return the single value.
reduce semantics [x] = return (interp semantics $ x)
-- Otherwise...
reduce semantics xs =
    -- For each possible sub-string...
    toTree (viewSubstrings xs) >>= \case
        -- If we have a term of type x, right next to a term of type x' -> y
        -- where x <= x', apply the two terms.
        (a, [Annotated t1 x, Annotated t2 (RightArrow x' y)], b)
            | x <= x' && isPartialPred t2 -> reduce semantics (a++[Annotated (applyPartialTerm t2 t1) y]++b)
            | x <= x' && otherwise -> reduce semantics (a++[Annotated (App t2 [t1]) y]++b)
        -- If we have a term of type (x <- y) next to a term of type y' where
        -- y' <= y, apply the two terms
        (a, [Annotated t1 (LeftArrow x y), Annotated t2 y'], b)
            | y >= y' && isPartialPred t1 -> reduce semantics (a++[Annotated (applyPartialTerm t1 t2) x]++b)
            | y >= y' && otherwise -> reduce semantics (a++[Annotated (App t1 [t2]) x]++b)
        -- Otherwise, we no longer consier this branch. 
        _ -> Node () []

-- | Get all parses of a string with regard to a lexicon.
getAllParses :: forall a t x. (PartialOrd t, Eq x) => MontagueSemantics a t x -> Proxy a -> String -> [x]
getAllParses semantics aT xs = nub parses
  where
    parses = join $ bfs $
        fmap (bfs . reduce semantics) (
        annotate semantics aT xs)

-- | Get a single parse of a string with regard to a lexicon.
getParse :: (Eq x, PartialOrd t) => MontagueSemantics a t x -> Proxy a -> String -> Maybe x
getParse semantics aT xs = listToMaybe $ getAllParses semantics aT xs

-- | Get a single parse of a string with regard to some explicitly passed lexicon.
getParseFromLexicon :: SomeLexicon -> (forall a t. (Show a, Show t) => AnnotatedTerm a t -> r) -> String -> Maybe r
getParseFromLexicon (SomeLexicon _ aT _ _ semantics) f input = f <$> getParse semantics aT input

-- | Get all parses of a string with regard to a lexicon.
getAllParsesFromLexicon :: SomeLexicon -> (forall a t. (Show a, Show t) => AnnotatedTerm a t -> r) -> String -> [r]
getAllParsesFromLexicon (SomeLexicon _ aT _ _ semantics) f input = f <$> getAllParses semantics aT input