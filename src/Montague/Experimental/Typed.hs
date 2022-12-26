
{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies, GADTs, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, LambdaCase #-}

module Montague.Experimental.Typed where

-- An experimental module with a more strongly typed representation of Montague semantics.
-- This also will include things like indexicals and possible world semantics.

import Montague.Experimental.Worlds
import Montague.Experimental.Prop4
import GHC.Types
import Data.Data
import Control.Monad.Coroutine
import Data.Functor.Identity
import Control.Monad.Coroutine.SuspensionFunctors (Yield)

data LambekType = 
    T Type 
  | L LambekType LambekType
  | R LambekType LambekType

type Sentence = 'T Bool

class BooleanType _Ω a where
  coord :: (LambekTerm _Ω (T _Ω) -> LambekTerm _Ω (T _Ω) -> LambekTerm _Ω (T _Ω)) 
    -> LambekTerm _Ω a -> LambekTerm _Ω a -> LambekTerm _Ω a

instance BooleanType _Ω (T _Ω) where
  coord f x y = f x y

instance BooleanType _Ω b => BooleanType _Ω (L a b) where
  coord f x y = LLamL $ \v -> coord f (LAppL v x) (LAppL v y)

instance BooleanType _Ω b => BooleanType _Ω (R a b) where
  coord f x y = LLamR $ \v -> coord f (LAppR x v) (LAppR y v)

-- | Type family to get the semantic interpretation of a lambek
-- type.
type family Bracket (x :: LambekType) :: Type where
  Bracket (L a b)  = Bracket a -> Bracket b
  Bracket (R a b)  = Bracket a -> Bracket b
  Bracket (T a) = a

-- | A type of terms abstracted over a type of "truth values"
-- _Ω.
--
-- The symbol Ω was chosen to evoke the similarity of the 
--  subobject classifier in topos theory.
data Term _Ω a where
    Lam  :: (Term _Ω a -> Term _Ω b) -> Term _Ω (a -> b)
    App  :: Term _Ω (a -> b) -> Term _Ω a -> Term _Ω b
    Atom :: Typeable a => a -> Term _Ω a
    And  :: Term _Ω _Ω -> Term _Ω _Ω -> Term _Ω _Ω
    Or   :: Term _Ω _Ω -> Term _Ω _Ω -> Term _Ω _Ω
    All  :: (Term _Ω a    -> Term _Ω _Ω) -> Term _Ω _Ω
    Some :: (Term _Ω _Ω -> Term _Ω _Ω) -> Term _Ω _Ω
    Will :: Term _Ω _Ω -> Term _Ω _Ω
    WillAlways :: Term _Ω _Ω -> Term _Ω _Ω

data SomeTerm _Ω = forall a. Typeable a => SomeTerm (Term _Ω a)

subst :: [(String, SomeTerm _Ω)] -> Term _Ω a -> Term _Ω a
subst [] t = t
subst ((x, y):xs) t = subst xs (substVar x y t)

substVar :: String -> SomeTerm _Ω -> Term _Ω a -> Term _Ω a
substVar x y t = undefined

unify :: Term _Ω a -> Term _Ω a -> [(String, SomeTerm _Ω)]
unify = undefined

-- | The world is the totality of facts, not of things. -- Wittgenstein
type Facts _Ω w = [Term _Ω (w -> Bool)]

data LambekTerm _Ω a where
    LLamL  :: (LambekTerm _Ω a -> LambekTerm _Ω b) -> LambekTerm _Ω (L a b)
    LLamR  :: (LambekTerm _Ω a -> LambekTerm _Ω b) -> LambekTerm _Ω (R a b)
    LAppL  :: LambekTerm _Ω a -> LambekTerm _Ω (L a b) -> LambekTerm _Ω b
    LAppR  :: LambekTerm _Ω (R a b) -> LambekTerm _Ω a -> LambekTerm _Ω b
    LAtom  :: (Show a, Typeable a) => a -> LambekTerm _Ω (T a)
    LAnd   :: LambekTerm _Ω (T _Ω) -> LambekTerm _Ω (T _Ω) -> LambekTerm _Ω (T _Ω)
    LOr    :: LambekTerm _Ω (T _Ω) -> LambekTerm _Ω (T _Ω) -> LambekTerm _Ω (T _Ω)
    LAll   :: (LambekTerm _Ω a -> LambekTerm _Ω (T _Ω)) -> LambekTerm _Ω (T _Ω)
    LSome  :: (LambekTerm _Ω (T _Ω) -> LambekTerm _Ω (T _Ω)) 
       -> LambekTerm _Ω (T _Ω)
    LVar   :: String -> Proxy a -> LambekTerm _Ω a

instance (Show _Ω) => Show (LambekTerm _Ω a) where
  show = \case
    LLamL f   -> "λₗx."  ++ (show $ f (LVar "x" Proxy)) -- TODO: Make sure variables are not captured here.
    LLamR f   -> "λᵣx." ++ (show $ f (LVar "x" Proxy)) -- TODO: Make sure variables are not captured here.
    LAppL f x -> (show f) ++ " " ++ (show x)
    LAppR f x -> (show f) ++ " " ++ (show x)
    LAtom x   -> show x
    LAnd x y  -> (show x) ++ " ∧ " ++ (show y)
    LOr x y   -> (show x) ++ " ∨ " ++ (show y)
    LAll f    -> "∀x." ++ (show $ f (LVar "x" Proxy)) -- TODO: Make sure variables are not captured here.
    LSome f   -> "∃x." ++ (show $ f (LVar "x" Proxy)) -- TODO: Make sure variables are not captured here.

instance Eq a => Eq (LambekTerm _Ω (T a)) where
  (LAtom x) == (LAtom y) = x == y
  _ == _ = False

-- Example from SEP:
every :: Term _Ω ((a -> _Ω) -> (a -> _Ω) -> _Ω)
every = Lam $ \p -> Lam $ \q -> All $ \x ->
  And (App p x) (App q x)

data Person = Nate | William | Michael | Andrew deriving(Eq, Show)

-- | Here we can define a term in terms of a concrete semantics,
-- but how do we define a term that can depend on the current list of
-- facts? 
-- 
-- I guess we could have the set of "facts" be part of the "world",
-- and recursively reference (maybe via an implicit parameter)
-- the search procedure in light of the current set of rules.
likes :: LambekTerm Bool (L (T Person) (R (T Person) (T Bool)))
likes = LLamL $ \x -> LLamR $ \y -> LAtom $
    x == nate && y == william || x == william && y == nate

-- nate :: LambekTerm _Ω (T Person)
nate = LAtom Nate

-- william :: LambekTerm _Ω (T Person)
william = LAtom William

-- example :: LambekTerm _Ω (T _Ω)
example = nate `LAppL` likes `LAppR` william

example2 = (nate `LAppL` likes `LAppR` william) 
    `Montague.Experimental.Typed.and` 
      (william `LAppL` likes `LAppR` nate)

-- TODO: Coordination via type-raising.
-- example3 = (nate `Montague.Experimental.Typed.and` will) `LAppL` 
--  likes `LAppR` michael

-- | -ed morpeme: play-ed -> played. am-ed -> was. see-ed -> saw.
ed :: Term _Ω _Ω -> Term _Ω _Ω
ed = undefined

-- | "will" -- a future tense modal operator over possible worlds.
-- 
-- In terms of Kripke semantics, this is the "possibility" operator
--  over the "future worlds" relation.
--
will :: Term _Ω _Ω -> Term _Ω _Ω
will = Will

-- | "will always" -- a future tense modal operator over possible worlds.
-- 
-- In terms of Kripke semantics, this is the "nescesity" operator
--  over the "future worlds" relation.
--
willAlways :: Term _Ω _Ω -> Term _Ω _Ω
willAlways = WillAlways

-- | Here is what an evaluation function for our algebra could look like.
-- We can try to evaluate universal and existential propositions,
-- and "fail" if they are not definitive.
-- 
-- Returns a coroutine which can be run indefinitely to continue producing 
-- intermediate results.
eval :: Term _Ω _Ω -> Coroutine (Yield Bool) Identity Bool
eval = undefined

and :: BooleanType _Ω a => LambekTerm _Ω a -> LambekTerm _Ω a -> LambekTerm _Ω a
and x y = coord LAnd x y

or :: BooleanType _Ω a => LambekTerm _Ω a -> LambekTerm _Ω a -> LambekTerm _Ω a
or x y = coord LOr x y

-- | Get the current (partial) truth value from a coroutine.
currentValue :: Coroutine (Yield Bool) Identity Bool -> Prop4
currentValue = undefined