
{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies, GADTs, FlexibleInstances, FlexibleContexts #-}

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

class BooleanType a where
  coord :: (LambekTerm Sentence -> LambekTerm Sentence -> LambekTerm Sentence) -> LambekTerm a -> LambekTerm a -> LambekTerm a

instance BooleanType Sentence where
  coord f x y = f x y

instance BooleanType b => BooleanType (L a b) where
  coord f x y = LLamL $ \v -> coord f (LAppL v x) (LAppL v y)

instance BooleanType b => BooleanType (R a b) where
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

data LambekTerm a where
    LLamL  :: (LambekTerm a -> LambekTerm b) -> LambekTerm (L a b)
    LLamR  :: (LambekTerm a -> LambekTerm b) -> LambekTerm (R a b)
    LAppL  :: LambekTerm a -> LambekTerm (L a b) -> LambekTerm b
    LAppR  :: LambekTerm (R a b) -> LambekTerm a -> LambekTerm b
    LAtom  :: Typeable a => a -> LambekTerm (T a)
    LAnd   :: LambekTerm Sentence -> LambekTerm Sentence -> LambekTerm Sentence
    LOr    :: LambekTerm Sentence -> LambekTerm Sentence -> LambekTerm Sentence
    LAll   :: (LambekTerm a -> LambekTerm Sentence) -> LambekTerm Sentence
    LSome  :: (LambekTerm Sentence -> LambekTerm Sentence) 
       -> LambekTerm Sentence

instance Eq a => Eq (LambekTerm (T a)) where
  (LAtom x) == (LAtom y) = x == y
  _ == _ = False

-- Example from SEP:
every :: Term _Ω ((a -> _Ω) -> (a -> _Ω) -> _Ω)
every = Lam $ \p -> Lam $ \q -> All $ \x ->
  And (App p x) (App q x)

data Person = Nate | William | Michael | Andrew deriving(Eq)

-- | Here we can define a term in terms of a concrete semantics,
-- but how do we define a term that can depend on the current list of
-- facts? 
-- 
-- I guess we could have the set of "facts" be part of the "world",
-- and recursively reference (maybe via an implicit parameter)
-- the search procedure in light of the current set of rules.
likes :: LambekTerm (L (T Person) (R (T Person) Sentence))
likes = LLamL $ \x -> LLamR $ \y -> LAtom $
    x == nate && y == william || x == william && y == nate

nate :: LambekTerm (T Person)
nate = LAtom Nate

william :: LambekTerm (T Person)
william = LAtom William

example :: LambekTerm Sentence
example = nate `LAppL` likes `LAppR` william

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

and :: BooleanType a => LambekTerm a -> LambekTerm a -> LambekTerm a
and x y = coord LAnd x y

or :: BooleanType a => LambekTerm a -> LambekTerm a -> LambekTerm a
or x y = coord LOr x y

-- | Get the current (partial) truth value from a coroutine.
currentValue :: Coroutine (Yield Bool) Identity Bool -> Prop4
currentValue = undefined