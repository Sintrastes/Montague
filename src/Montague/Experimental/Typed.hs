
{-# LANGUAGE ScopedTypeVariables, TypeOperators, DataKinds, KindSignatures, TypeApplications, 
   TypeFamilies, GADTs, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, LambdaCase #-}

module Montague.Experimental.Typed where

-- An experimental module with a more strongly typed representation of Montague semantics.
-- This also will include things like indexicals and possible world semantics.

import Montague.Experimental.Worlds
import Montague.Experimental.Prop4
import GHC.Types
import Type.Reflection (TypeRep, Typeable, typeOf)
import Data.Data (Proxy(..))
import Control.Monad.Coroutine
import Data.Functor.Identity
import Control.Monad.Coroutine.SuspensionFunctors (Yield)

data LambekType where 
    T       :: Type -> LambekType
    L       :: LambekType -> LambekType -> LambekType
    R       :: LambekType -> LambekType -> LambekType
    Extract :: LambekType -> LambekType -> LambekType
    Scoped  :: LambekType -> LambekType -> LambekType
    Conj    :: LambekType -> LambekType -> LambekType
    Disj    :: LambekType -> LambekType -> LambekType

type (∧) = Conj
type (∨) = Disj
type (/) = L
type (\\) = R
type (↑) = Extract
type (⇑) = Scoped

instance Show LambekType where
  show = \case
    T x         -> "" -- TODO: show $ typeRep x
    R x y       -> (show x) ++ "\\" ++ (show y)
    L x y       -> (show x) ++ "/"  ++ (show y)
    Conj x y    -> (show x) ++ "∧"  ++ (show y)
    Disj x y    -> (show x) ++ "∨"  ++ (show y)
    Extract x y -> (show x) ++ "↑"  ++ (show y)
    Scoped x y  -> (show x) ++ "⇑"  ++ (show y)

type Sentence = 'T Bool

class BooleanType _Ω a where
  coord :: (_Ω -> _Ω -> _Ω) 
    -> Term _Ω a -> Term _Ω a -> Term _Ω a

instance BooleanType _Ω (T _Ω) where
  coord f (Atom x) (Atom y) = Atom $ f x y

instance BooleanType _Ω b => BooleanType _Ω (L b a) where
  coord f x y = LamL $ \v -> coord f (AppL x v) (AppL y v)

instance BooleanType _Ω b => BooleanType _Ω (R a b) where
  coord f x y = LamR $ \v -> coord f (AppR v x) (AppR v y)

-- | Type family to get the semantic interpretation of a lambek
-- type.
type family Bracket (x :: LambekType) :: Type where
  Bracket (L a b)  = Bracket a -> Bracket b
  Bracket (R a b)  = Bracket a -> Bracket b
  Bracket (T a)    = a
  Bracket (a ∧ b)  = (Bracket a, Bracket b)
  Bracket (a ∨ b)  = Either (Bracket a) (Bracket b)
  Bracket (a ↑ b) = (Bracket a) -> (Bracket b)
  Bracket (a ⇑ b) = (Bracket a -> Bracket b) -> Bracket b

-- | A type of terms in the lambek calculus abstracted over a type
-- of "truth values" _Ω.
--
-- The symbol Ω was chosen to evoke the similarity of the 
--  subobject classifier in topos theory.
data Term _Ω a where
    LamL  :: (Term _Ω b -> Term _Ω a) -> Term _Ω (L a b)
    LamR  :: (Term _Ω a -> Term _Ω b) -> Term _Ω (R a b)
    AppL  :: Term _Ω (b / a) -> Term _Ω a -> Term _Ω b
    AppR  :: Term _Ω a -> Term _Ω (a \\ b) -> Term _Ω b
    Atom  :: (Show a, Typeable a) => a -> Term _Ω (T a)
    -- And   :: Term _Ω (T _Ω) -> Term _Ω (T _Ω) -> Term _Ω (T _Ω)
    -- Or    :: Term _Ω (T _Ω) -> Term _Ω (T _Ω) -> Term _Ω (T _Ω)
    -- All   :: (Term _Ω a -> Term _Ω (T _Ω)) -> Term _Ω (T _Ω)
    -- Some  :: (Term _Ω (T _Ω) -> Term _Ω (T _Ω)) 
    --   -> Term _Ω (T _Ω)
    TVar   :: String -> Proxy a -> Term _Ω a

-- | An example type of raw semantic expressions.
data Sem = 
    Pred String [Sem]
  | Individual Person
  | And Sem Sem
  | Or Sem Sem
  | All (Sem -> Sem)
  | Some (Sem -> Sem)
  | Not Sem
  | Var String

-- | The world is the totality of facts, not of things. -- Wittgenstein
type Facts _Ω = [Term _Ω (T _Ω)]

data SomeTerm _Ω = forall a. Typeable a => SomeTerm (Term _Ω a)

subst :: [(String, SomeTerm _Ω)] -> Term _Ω a -> Term _Ω a
subst [] t = t
subst ((x, y):xs) t = subst xs (substVar x y t)

substVar :: String -> SomeTerm _Ω -> Term _Ω a -> Term _Ω a
substVar x y t = undefined

unify :: Term _Ω a -> Term _Ω a -> [(String, SomeTerm _Ω)]
unify = undefined

instance (Show _Ω) => Show (Term _Ω a) where
  show = \case
    LamL f   -> "λₗx."  ++ show (f (TVar "x" Proxy)) -- TODO: Make sure variables are not captured here.
    LamR f   -> "λᵣx." ++ show (f (TVar "x" Proxy)) -- TODO: Make sure variables are not captured here.
    AppL f x -> show f ++ " " ++ show x
    AppR f x -> show f ++ " " ++ show x
    Atom x   -> show x

instance Show Sem where
    show = \case
      And x y  -> show x ++ " ∧ " ++ show y
      Or x y   -> show x ++ " ∨ " ++ show y
      All f    -> "∀x." ++ show (f (Var "x")) -- TODO: Make sure variables are not captured here.
      Some f   -> "∃x." ++ show (f (Var "x")) -- TODO: Make sure variables are not captured here.

instance Eq a => Eq (Term _Ω (T a)) where
  (Atom x) == (Atom y) = x == y
  _ == _ = False

-- | "Type-raising" operator. Converts a type a to a type b/(a\b).
-- Useful for making non-boolean types boolean for the sake of coordination.
raise :: Term _Ω a -> Term _Ω (b/(a\\b))
raise x = LamL $ \(v :: Term _Ω (a\\b)) -> AppR x v

-- | Function ("proof") witnessing the associativity of the lambek
--  calculus.
assoc :: Term _Ω ((a \\ b) / c) -> Term _Ω (a \\ (b / c))
assoc x = LamR $ \y -> LamL $ \z -> y `AppR` (x `AppL` z)

-- | Function ("proof") witnessing the associativity of the lambek
--  calculus in the opposite direction as assoc.
unassoc :: Term _Ω (a \\ (b / c)) -> Term _Ω ((a \\ b) / c)
unassoc x = LamL $ \z -> LamR $ \y -> (y `AppR` x) `AppL` z

-- Example from SEP:
--  Needs to be re-worked with the approach from Carpenter.
-- every :: Term _Ω ((a -> (T _Ω)) (T _Ω) (a -> (T _Ω))
-- every = Lam $ \p -> Lam $ \q -> All $ \x ->
--  And (App p x) (App q x)

data Person = Nate | William | Michael | Andrew deriving(Eq, Show)

-- | Here we can define a term in terms of a concrete semantics,
-- but how do we define a term that can depend on the current list of
-- facts? 
-- 
-- I guess we could have the set of "facts" be part of the "world",
-- and recursively reference (maybe via an implicit parameter)
-- the search procedure in light of the current set of rules.
likes :: Term Sem ((T Person \\ T Sem) / T Person)
likes = LamL $ \x -> LamR $ \y -> Atom $
    Pred "likes" [Individual Nate, Individual William] `And` 
      Pred "likes" [Individual William, Individual Nate]
    -- This was the previous "direct" interpretation:
         -- x == nate && y == william || x == william && y == nate

-- nate :: Term _Ω (T Person)
nate = Atom Nate

-- william :: Term _Ω (T Person)
william = Atom William

michael = Atom Michael

-- example :: Term _Ω (T _Ω)
example = nate `AppR` (likes `AppL` william)

-- Coordination
example2 = (nate `AppR` (likes `AppL` william))
    `Montague.Experimental.Typed.and` 
     (william `AppR` (likes `AppL` nate))

-- Coordination via type-raising.
example3 = raise nate
   `Montague.Experimental.Typed.and` raise william
   `AppL` (likes `AppL` michael)

-- | -ed morpeme: play-ed -> played. am-ed -> was. see-ed -> saw.
ed :: Term _Ω (T _Ω) -> Term _Ω (T _Ω)
ed = undefined

-- | "will" -- a future tense modal operator over possible worlds.
-- 
-- In terms of Kripke semantics, this is the "possibility" operator
--  over the "future worlds" relation.
--
-- will :: Term _Ω (T _Ω) -> Term _Ω (T _Ω)
-- will = Will

-- | "will always" -- a future tense modal operator over possible worlds.
-- 
-- In terms of Kripke semantics, this is the "nescesity" operator
--  over the "future worlds" relation.
--
-- willAlways :: Term _Ω (T _Ω) -> Term _Ω (T _Ω)
-- willAlways = WillAlways

-- | Here is what an evaluation function for our algebra could look like.
-- We can try to evaluate universal and existential propositions,
-- and "fail" if they are not definitive.
-- 
-- Returns a coroutine which can be run indefinitely to continue producing 
-- intermediate results.
-- eval :: Term _Ω (T _Ω) -> Coroutine (Yield Bool) Identity Bool
-- eval = undefined

eval :: Term _Ω a -> Bracket a
eval = undefined

-- TODO: There should probably be a typeclass for those to make this generic. 
and :: BooleanType Sem a => Term Sem a -> Term Sem a -> Term Sem a
and x y = coord And x y

or :: BooleanType Sem a => Term Sem a -> Term Sem a -> Term Sem a
or x y = coord Or x y

-- | Get the current (partial) truth value from a coroutine.
currentValue :: Coroutine (Yield Bool) Identity Bool -> Prop4
currentValue = undefined