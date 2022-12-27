
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
import Montague.Experimental.LambekType
import Data.List
import Control.Exception (AllocationLimitExceeded)

type Sentence = 'T Bool

class BooleanType _Ω a where
  coord :: (_Ω -> _Ω -> _Ω) 
    -> Term a -> Term a -> Term a

instance BooleanType _Ω (T _Ω) where
  coord f (Atom x) (Atom y) = Atom $ f x y

instance BooleanType _Ω b => BooleanType _Ω (L b a) where
  coord f x y = LamL $ \v -> coord f (x <. v) (y <. v)

instance BooleanType _Ω b => BooleanType _Ω (R a b) where
  coord f x y = LamR $ \v -> coord f (v .> x) (v .> y)

-- | Type family to get the semantic interpretation of a lambek
-- type.
type family Bracket (x :: LambekType) :: Type where
  Bracket (L a b)  = Bracket b -> Bracket a
  Bracket (R a b)  = Bracket a -> Bracket b
  Bracket (N a b)  = a -> b
  Bracket (T a)    = a
  Bracket (a ∧ b)  = (Bracket a, Bracket b)
  Bracket (a ∨ b)  = Either (Bracket a) (Bracket b)
  Bracket (a ↑ b)  = Bracket a -> Bracket b
  Bracket (a ⇑ b)  = (Bracket a -> Bracket b) -> Bracket b

-- eval :: Term x -> Bracket x
-- eval = \case
--   Atom x -> x
--   LamL f -> \x -> let y = Atom x in _

-- Maybe this would work better with functional dependencies?
-- class Bracket (x :: LambekType) a | x -> a where
--     eval :: Term x -> a

-- | A type of terms in the lambek calculus
data Term a where
    LamL  :: (Term b -> Term a) -> Term (a / b)
    LamR  :: (Term a -> Term b) -> Term (a \\ b)
    -- I'm not exactly sure how this rule would work in a GADT.
    --
    -- The idea is that if we can choose some (possibly empty)
    --  terms l and r that concatenate to a, then we can construct
    --  a ↑ b
    LamS  :: (Term b -> Term a) -> Term (a ↑ b)
    -- | Constructor for building a new noun from a function from
    --  ind -> o.
    LamN  :: (Term (NP ind) -> Term (S o)) -> Term (N ind o)
    AppL  :: Term (b / a) -> Term a -> Term b
    AppR  :: Term a -> Term (a \\ b) -> Term b
    -- | Constructor for applying scoped functions.
    --
    -- Note: NOT a valid rule in the calculus, only used
    -- internally.
    AppS  :: Term (a ↑ b) -> Term b -> Term a
    Atom  :: (Show a, Typeable a) => a -> Term (T a)
    TVar   :: String -> Proxy a -> Term a

(<.) = AppL
(.>) = AppR

-- | An example type of raw semantic expressions.
data Sem = 
    Pred String [Sem]
  | Individual Person
  | Subject Subject
  | And Sem Sem
  | Or Sem Sem
  | All (Sem -> Sem)
  | Some (Sem -> Sem)
  | Not Sem
  | Var String
  | The Sem
  | Const Bool

-- | Semantics for the definite determiner.
ι = The

-- | The world is the totality of facts, not of things. -- Wittgenstein
type Facts _Ω = [Term (T _Ω)]

data SomeTerm _Ω = forall a. Typeable a => SomeTerm (Term a)

subst :: [(String, SomeTerm _Ω)] -> Term a -> Term a
subst [] t = t
subst ((x, y):xs) t = subst xs (substVar x y t)

substVar :: String -> SomeTerm _Ω -> Term a -> Term a
substVar x y t = undefined

unify :: Term a -> Term a -> [(String, SomeTerm _Ω)]
unify = undefined

instance Show (Term a) where
  show = \case
    LamL f   -> "λₗx."  ++ show (f (TVar "x" Proxy)) -- TODO: Make sure variables are not captured here.
    LamR f   -> "λᵣx." ++ show (f (TVar "x" Proxy)) -- TODO: Make sure variables are not captured here.
    AppL f x -> show f ++ " " ++ show x
    AppR f x -> show f ++ " " ++ show x
    Atom x   -> show x

instance Show Sem where
    show = \case
      Pred p xs -> p ++ "(" ++ intercalate ", " (show <$> xs) ++ ")"
      Var x -> x
      And x y   -> show x ++ " ∧ " ++ show y
      Or x y    -> show x ++ " ∨ " ++ show y
      All f     -> "∀x." ++ show (f (Var "x")) -- TODO: Make sure variables are not captured here.
      Some f    -> "∃x." ++ show (f (Var "x")) -- TODO: Make sure variables are not captured here.
      The x     -> "ι(" ++ show x ++ ")"

instance Eq a => Eq (Term (T a)) where
  (Atom x) == (Atom y) = x == y
  _ == _ = False

-- | "Type-raising" operator. Converts a type a to a type b/(a\b).
-- Useful for making non-boolean types boolean for the sake of coordination.
raise :: Term a -> Term (b/(a\\b))
raise x = LamL $ \v -> AppR x v

-- | Variant of type-raising operator "from the other side".
raise' :: Term a -> Term ((b/a)\\b)
raise' x = LamR $ \v -> AppL v x

-- | Function ("proof") witnessing the associativity of the lambek
--  calculus.
assoc :: Term ((a \\ b) / c) -> Term (a \\ (b / c))
assoc x = LamR $ \y -> LamL $ \z -> y .> (x <. z)

-- | Function ("proof") witnessing the associativity of the lambek
--  calculus in the opposite direction as assoc.
unassoc :: Term (a \\ (b / c)) -> Term ((a \\ b) / c)
unassoc x = LamL $ \z -> LamR $ \y -> (y .> x) <. z

-- | Variant of unassoc for type-raised arguments.
-- except I don't think this makes sense at all...
-- unassoc' :: Term (a / (b \\ c)) -> Term ((a / b) \\ c) 
-- unassoc' x = LamR $ \y -> y

-- Example from SEP:
--  Needs to be re-worked with the approach from Carpenter.
-- every :: Term _Ω ((a -> (T _Ω)) (T _Ω) (a -> (T _Ω))
-- every = Lam $ \p -> Lam $ \q -> All $ \x ->
--  And (App p x) (App q x)

data Person = 
      Nate 
    | William 
    | Michael 
    | Andrew 
    | Socrates 
  deriving(Eq, Show)

data Subject =
      Food
    | Cooking
    | Gardening
    | Sports
    | Philosophy
    | Politics
    | Anime
    | Movies
    | Hiking
  deriving(Eq, Show)

cooking = Atom Cooking
anime = Atom Anime

-- | Here we can define a term in terms of a concrete semantics,
-- but how do we define a term that can depend on the current list of
-- facts? 
-- 
-- I guess we could have the set of "facts" be part of the "world",
-- and recursively reference (maybe via an implicit parameter)
-- the search procedure in light of the current set of rules.
likes :: Term ((NP Person \\ S Sem) / NP Person)
likes = LamL $ \(Atom x) -> LamR $ \(Atom y) -> Atom $
    Pred "likes" [Individual x, Individual y]

-- | Likes, in the sense of a person and a subject.
likes2 :: Term ((NP Person \\ S Sem) / NP Subject)
likes2 = LamL $ \(Atom x) -> LamR $ \(Atom y) -> Atom $
    Pred "likes" [Individual y, Subject x]

-- | Definite determiner -- converts a noun into a
-- noun phrase.
the :: Term (N a Sem) -> Term (NP a)
the (LamN x) = undefined -- Atom (ι x) 

-- | An example of a noun which is not a proper noun.
man :: Term (N Person Sem)
man = LamN $ \x -> 
  Atom $ Const True

-- who (or which, since we do not distinguish animacy), used to construct relative
--  clauses.
who :: Term (N n Sem \\ N n Sem / (S Sem ↑ NP n))
who = LamL $ \v -> LamR $ \p -> LamN $ \x -> let 
     Atom a1 = v `AppS` x
     LamN p' = p
     Atom a2 = p' x
  in
     Atom $ And a1 a2 

relativeClauseExample :: Term (S Sem ↑ NP Person)
relativeClauseExample = LamS $ \x ->
    x .> (likes <. william)

-- "the man who likes William"
relativeClauseUsage :: Term (NP Person)
relativeClauseUsage = the $ man .> (who <. LamS (\x -> x .> (likes <. william)))

-- "nate likes william and michael"
rightCoordinationExample = (nate .> assoc likes) 
  .> (raise' william `and'` raise' michael)

-- "the man who likes cooking and anime"
relativeClauseUsage2 = the $ 
  man .> (who <. 
    LamS (\x -> (x .> assoc likes2) 
        .> (raise' cooking `and'` raise' anime)))

nate :: Term (NP Person)
nate = Atom Nate

william :: Term (NP Person)
william = Atom William

michael = Atom Michael

example :: Term (T Sem)
example = nate .> (likes <. william)

-- Coordination
example2 = (nate .> (likes <. william)) `and'` (william .> (likes <. nate))

-- Coordination via type-raising.
example3 = raise nate `and'` raise william <. (likes <. michael)

-- | -ed morpeme: play-ed -> played. am-ed -> was. see-ed -> saw.
ed :: Term (T _Ω) -> Term (T _Ω)
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

-- TODO: There should probably be a typeclass for those to make this generic. 
and' :: BooleanType Sem a => Term a -> Term a -> Term a
and' x y = coord And x y

or :: BooleanType Sem a => Term a -> Term a -> Term a
or x y = coord Or x y

-- | Get the current (partial) truth value from a coroutine.
currentValue :: Coroutine (Yield Bool) Identity Bool -> Prop4
currentValue = undefined