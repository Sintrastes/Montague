
module Montague.Types where

import Prelude hiding ((<=),(>=))
import Control.Monad.Tree
import Data.PartialOrd

type NonDet a = Tree () [] a

-- | Set of types for a lambek grammar given a set of base types t.
data LambekType t =
    RightArrow (LambekType t) (LambekType t)
  | LeftArrow (LambekType t) (LambekType t)
  | BasicType t deriving(Eq)

instance PartialOrd t => PartialOrd (LambekType t) where
    BasicType x <= BasicType y = x <= y
    (RightArrow x y) <= (RightArrow x' y') = x >= x' && y <= y'
    (LeftArrow x y) <= (LeftArrow x' y') = x >= x' && y <= y'
    _ <= _ = False

type MontagueType t = NonDet (LambekType t)

instance Show a => Show (LambekType a) where
  show (RightArrow x y) = "(" ++ show x ++ " → " ++ show y ++ ")"
  show (LeftArrow x y)  = "(" ++ show x ++ " ← " ++ show y ++ ")"
  show (BasicType x) = show x

-- | Data type for a generic typed term returned as the result of
-- parsing a natural language statement, given a type a
-- of atoms, and a type t of types.
data Term a t =
    Atom a
  | App (Term a t) [Term a t] deriving(Eq)

instance Show a => Show (Term a t) where
  show (Atom xs) = show xs
  show (App x xs) = show x ++ "(" ++ foldr1 (\ x y -> x ++ "," ++ y) (map show xs) ++ ")"

type MontagueTerm a t = NonDet (Term a t)

-- | Helper function for calculating the types of lambek terms
flatten :: LambekType a -> [LambekType a]
flatten (BasicType s) = [BasicType s]
flatten (RightArrow x y) = x : flatten y
flatten (LeftArrow x y) = y : flatten x

-- | Helper function to check if a predicate has been applied.
isPartialPred :: Term a t -> Bool
isPartialPred (App _ _)  = True
isPartialPred (Atom _) = False

-- | Partial function that turns a curried term f(x)(y) into one of the form f(x,y)
applyPartialTerm (App x xs) y = App x (xs ++ [y])

data AnnotatedTerm a t = Annotated (Term a t) (LambekType t)
  deriving(Eq)

instance (Show a, Show t) => Show (AnnotatedTerm a t) where
  show (Annotated term typ) = show term ++ ": " ++ show typ

type Lexicon a t = String -> MontagueTerm a t

----------------- Helper functions for building up types from trees.

-- | Helper function to combine two nondeterministic types with a left arrow.
leftArrow :: NonDet (LambekType t) -> NonDet (LambekType t) -> NonDet (LambekType t)
leftArrow x y = do
    x' <- x
    y' <- y
    return $ x' `LeftArrow` y'

-- | Helper function to combine two nondeterministic types with a right arrow.
rightArrow :: NonDet (LambekType a) -> NonDet (LambekType a) -> NonDet (LambekType a)
rightArrow x y = do
    x' <- x
    y' <- y
    return $ x' `RightArrow` y'