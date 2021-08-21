
module Montague where

import Control.Monad.Tree
import Control.Monad
import Data.Maybe
import Data.List

-------- Helper functions ---------
split :: Char -> String -> [String]
split c s = case rest of
    []     -> [chunk]
    _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

toTree xs = Node () (fmap Leaf xs)

-- | Set of types for a lambek grammar given a set of base types t.
data LambekType t =
    RightArrow (LambekType t) (LambekType t)
  | LeftArrow (LambekType t) (LambekType t)
  | BasicType t deriving(Eq)

instance Show a => Show (LambekType a) where
  show (RightArrow x y) = "(" ++ show x ++ "->" ++ show y ++ ")"
  show (LeftArrow x y)  = "(" ++ show x ++ "<-" ++ show y ++ ")"
  show (BasicType x) = show x

-- | Data type for a generic typed term returned as the result of
-- parsing a natural language statement, given a type a
-- of atoms, and a type t of types.
data Term a t =
    Atom a (LambekType t)
  | App (Term a t) [Term a t] deriving(Eq)

-- Helper function for calculating the types of lambek terms
flatten :: LambekType a -> [LambekType a]
flatten (BasicType s) = [BasicType s]
flatten (RightArrow x y) = [x] ++ flatten y
flatten (LeftArrow x y) = [y] ++ flatten x

-- | Helper function to check if a predicate has been fully applied.
isPartialPred :: Term a t -> Bool
isPartialPred (App _ _) = True
isPartialPred (Atom _ _) = False

-- Partial function that turns a curried term f(x)(y) into one of the form f(x,y)
applyPartialTerm (App x xs) y = App x (xs ++ [y])

instance (Show a, Show t) => Show (Term a t) where
  show (Atom xs _) = show xs
  show (App x xs) = show x ++ "(" ++ (foldr1 (\x -> \y -> x ++ "," ++ y) (map show xs)) ++ ")"

data AnnotatedTerm a t = Annotated (Term a t) (LambekType t)
  deriving(Eq)

instance (Show a, Show t) => Show (AnnotatedTerm a t) where
  show (Annotated term typ) = show term ++ ": " ++ show typ

type Lexicon t = String -> Tree () [] (LambekType t)

----------------- Helper functions for building up types from trees.

-- | Helper function to combine two nondeterministic types with a left arrow.
leftArrow :: Tree () [] (LambekType t) -> Tree () [] (LambekType t) -> Tree () [] (LambekType t)
leftArrow x y = do
    x' <- x
    y' <- y
    return $ x' `LeftArrow` y'

-- | Helper function to combine two nondeterministic types with a right arrow.
rightArrow :: Tree () [] (LambekType a) -> Tree () [] (LambekType a) -> Tree () [] (LambekType a)
rightArrow x y = do
    x' <- x
    y' <- y
    return $ x' `RightArrow` y'

-- | Create an annotated term from a lexicon and a string
annotate :: Lexicon t -> String -> Tree () [] [AnnotatedTerm String t]
annotate lex xs = do
    lexes <- mapM lex $ split ' ' xs
    return $ zipWith Annotated
                     (zipWith Atom (split ' ' xs)
                        lexes) lexes

-- | View pattern to use to split a string
viewSubstrings2 :: [a] -> [([a], [a], [a])]
viewSubstrings2 xs = fmap (\n -> splitTwoAt n xs) [0..l - 2]
    where l = length xs
          splitTwoAt n xs =
              let (start, ys) = splitAt n xs in
              let (middle,end) = splitAt 2 ys in
                    (start,middle,end)

-- | Reduce a list of terms by applying adjacent terms according to their types
reduce :: Eq t => [AnnotatedTerm String t] -> Tree () [] (AnnotatedTerm String t)
reduce [x] = return x
reduce xs =
    (toTree $ viewSubstrings2 xs) >>= \case
        (a, [Annotated t1 x, Annotated t2 (RightArrow x' y)], b)
            | x == x' && isPartialPred t2 -> reduce (a++[Annotated (applyPartialTerm t2 t1) y]++b)
            | x == x' && otherwise -> reduce (a++[Annotated (App t2 [t1]) y]++b)
        (a, [Annotated t1 (LeftArrow x y), Annotated t2 y'], b)
            | y == y' && isPartialPred t1 -> reduce (a++[Annotated (applyPartialTerm t1 t2) x]++b)
            | y == y' && otherwise -> reduce (a++[Annotated (App t1 [t2]) x]++b)
        _ -> Node () []

-- | Get all parses of a string with regard to a lexicon.
getAllParses :: Eq t => Lexicon t -> String -> [AnnotatedTerm String t]
getAllParses lex xs =
  let parses = join $ bfs $ fmap bfs $ fmap reduce $ annotate lex xs in
      nub $ parses

-- | Get a single parse of a string with regard to a lexicon.
getParse :: Eq t => Lexicon t -> String -> Maybe (AnnotatedTerm String t)
getParse = (listToMaybe .) . getAllParses
