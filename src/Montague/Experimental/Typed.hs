
{-# LANGUAGE ScopedTypeVariables, TypeOperators, DataKinds, KindSignatures, TypeApplications, 
   TypeFamilies, GADTs, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}

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
import Debug.Trace

type Sentence = 'T Bool

class BooleanType _Ω a where
  coord :: (Var _Ω -> Var _Ω -> _Ω) 
    -> Term a -> Term a -> Term a

instance BooleanType _Ω (T _Ω) where
  coord f (Atom x) (Atom y) = Atom $ Val $ f x y

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
    LamE  :: (Term b -> Term a) -> Term (a ↑ b)
    LamS  :: ((Term b -> Term a) -> Term a) -> Term (b ⇑ a)
    -- | Constructor for building a new noun from a function from
    --  ind -> o.
    LamN  :: (Term (NP ind) -> Term (S o)) -> Term (N ind o)
    AppL  :: Term (b / a) -> Term a -> Term b
    AppR  :: Term a -> Term (a \\ b) -> Term b
    -- | Constructor for applying extracted functions.
    --
    -- Note: NOT a valid rule in the calculus, only used
    -- internally.
    AppE  :: Term (a ↑ b) -> Term b -> Term a
    -- Also not sure if this is right or not.
    AppS  :: Term (b ⇑ a) -> (Term b -> Term a) -> Term a
    Atom  :: (Show a, Typeable a) => Var a -> Term (T a)
    TVar  :: String -> Term a

-- Debugging utility used to see the top level constructor.
toplevel = \case
  LamL _   -> "LamL"
  LamR _   -> "LamR"
  LamE _   -> "LamE"
  LamS _   -> "LamS"
  LamN _   -> "LamN"
  AppL _ _ -> "AppL"
  AppR _ _ -> "AppR"
  AppE _ _ -> "AppE"
  AppS _ _ -> "AppS"
  Atom _   -> "Atom"
  TVar _   -> "TVar"

-- | Evaluate a lambda expression to normal form by beta reduction.
eval :: Term a -> Term a
eval (AppL (LamL f) x) = eval $ f x
eval (AppR x (LamR f)) = eval $ f x
eval (AppR x f) = eval $ AppR (eval x) (eval f)
eval (AppE (LamE f) x) = eval $ f x
eval (AppS (LamS f) x) = eval $ f x
eval (AppS f x) = eval $ AppS (eval f) (eval <$> x)
eval x = x

evalAtom :: Show a => Typeable a => Term (T a) -> Term (T a)
evalAtom x = evalAtom' $ eval x

evalAtom' (TVar x) = Atom $ Var x
evalAtom' x = x

(<.) = AppL
(.>) = AppR

-- | An example type of raw semantic expressions.
data Sem where
    Pred       :: String -> [Sem] -> Sem
    Individual :: Var Person -> Sem
    Subject    :: Var Subject -> Sem
    And        :: Var Sem -> Var Sem -> Sem
    Or         :: Var Sem -> Var Sem -> Sem
    Implies    :: Var Sem -> Var Sem -> Sem
    All        :: (Var a -> Sem) -> Sem
    Exists     :: (Var a -> Sem) -> Sem
    Not        :: Sem -> Sem
    The        :: (Var a -> Sem) -> Sem
    Const      :: Bool -> Sem

class Eval e a | e -> a where
  evaluate :: e -> a

instance Eval Sem Bool where
  evaluate _ = True

data Var a = Var String | Val a

instance Show a => Show (Var a) where
  show (Var x) = x
  show (Val x) = show x

getVar (Val x) = x

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
    LamL f   -> "λₗx."  ++ show (f (TVar "x")) -- TODO: Make sure variables are not captured here.
    LamR f   -> "λᵣx." ++ show (f (TVar "x")) -- TODO: Make sure variables are not captured here.
    LamN f   -> "λx." ++ show (f (TVar "x"))
    LamE f   -> "λx." ++ show (f (TVar "x"))
    LamS f   -> "λx.S" -- ++ show (f (TVar "x" Proxy))
    AppL f x -> show f ++ "(" ++ show x ++ ")"
    AppR f x -> show f ++ "(" ++ show x ++ ")"
    AppS x f -> show (f (TVar "x")) ++ "(" ++ show x ++ ")"
    AppE f x -> show f ++ "(" ++ show x ++ ")"
    Atom x   -> show x
    TVar x   -> x

instance Show Sem where
    show = \case
      Pred p xs -> p ++ "(" ++ intercalate ", " (show <$> xs) ++ ")"
      And x y   -> show x ++ " ∧ " ++ show y
      Or x y    -> show x ++ " ∨ " ++ show y
      Implies x y -> show x ++ " → " ++ show y
      All f     -> "∀x." ++ show (f (Var "x")) -- TODO: Make sure variables are not captured here.
      Exists f  -> "∃x." ++ show (f (Var "x")) -- TODO: Make sure variables are not captured here.
      The x     -> "ι(" -- ++ show x ++ ")"
      Individual x -> show x
      Subject x    -> show x
      Const x -> show x

instance Eq a => Eq (Term (T a)) where
  (Atom (Val x)) == (Atom (Val y)) = x == y
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

class HasAttributes n a | n -> a where

data Person = 
      Nate 
    | William 
    | Michael 
    | Andrew 
    | Socrates 
  deriving(Eq, Show, Enum)

-- | Type of attributes that specifically apply
-- to persons.
data PersonAttrs = 
      Mortal
    | Man
    | Performing PersonEvent
  deriving(Eq, Show)

instance HasAttributes Person PersonAttrs

-- | Type of events that can be performed by persons.
data PersonEvent = 
      Running
    | Walking
    | Talking
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

cooking = Atom $ Val Cooking
anime = Atom $ Val Anime

-- | Here we can define a term in terms of a concrete semantics,
-- but how do we define a term that can depend on the current list of
-- facts? 
-- 
-- I guess we could have the set of "facts" be part of the "world",
-- and recursively reference (maybe via an implicit parameter)
-- the search procedure in light of the current set of rules.
likes :: Term ((NP Person \\ S Sem) / NP Person)
likes = LamL $ \(Atom x) -> LamR $ \(Atom y) -> Atom $ Val $
    Pred "likes" [Individual x, Individual y]

-- | Likes, in the sense of a person and a subject.
likes2 :: Term ((NP Person \\ S Sem) / NP Subject)
likes2 = LamL $ \(Atom x) -> LamR $ \(Atom y) -> Atom $ Val $
    Pred "likes" [Individual y, Subject x]

-- | Definite determiner -- converts a noun into a
-- noun phrase.
the :: forall a. 
      Typeable a 
   => Show a 
   => Enum a 
   => Eval Sem Bool 
   => Term (N a Sem) -> Term (NP a)
the (LamN p) = Atom (Val $ head holdsFor)
  where
    entities :: [a] = [toEnum 0 ..]
    holdsFor = filter (evalAtom . p . Atom . Val) entities 

    evalAtom :: Term (S Sem) -> Bool
    evalAtom (Atom (Val x)) = evaluate x
    evalAtom (Atom (Var _)) = False

-- | An example of a noun which is not a proper noun.
man :: Term (N Person Sem)
man = LamN $ \x -> let Atom x' = evalAtom x in
  Atom $ Val $ Pred (show Man) [Individual x']

-- who (or which, since we do not distinguish animacy), used to construct relative
--  clauses.
who :: Term (N n Sem \\ N n Sem / (S Sem ↑ NP n))
who = LamL $ \v -> LamR $ \p -> LamN $ \x -> let 
     Atom a1 = v `AppE` x
     LamN p' = p
     Atom a2 = p' x
  in
     Atom $ Val $ And a1 a2

relativeClauseExample :: Term (S Sem ↑ NP Person)
relativeClauseExample = LamE $ \x ->
    x .> (likes <. william)

-- "the man who likes William"
relativeClauseUsage :: Term (NP Person)
relativeClauseUsage = the $ man .> (who <. LamE (\x -> x .> (likes <. william)))

-- "nate likes william and michael"
rightCoordinationExample = (nate .> assoc likes) 
  .> (raise' william `and'` raise' michael)

-- "the man who likes cooking and anime"
relativeClauseUsage2 = the $ 
  man .> (who <. 
    LamE (\x -> (x .> assoc likes2) 
        .> (raise' cooking `and'` raise' anime)))

nate :: Term (NP Person)
nate = Atom $ Val Nate

william :: Term (NP Person)
william = Atom $ Val William

michael = Atom $ Val Michael

socrates = Atom $ Val Socrates

class Entity n o | n -> o where
  inj :: Var n -> o

instance Entity Person Sem where
  inj = Individual

-- | The copula, in the sense of attribution.
is :: Typeable n => Show n => Show a => Entity n Sem => HasAttributes n a => Term (NP n \\ S Sem / T a)
is = LamL $ \attr -> LamR $ \e -> let Atom entity = evalAtom e in
  Atom $ Val $ Pred (show attr) [inj entity]

every :: Typeable n => Show n => Term ((NP n ⇑ S Sem) / N n Sem)
every = LamL $ \p' -> LamS $ \q -> Atom $ Val $ 
    All $ \x -> let 
      LamN p = p'
      e1 = evalAtom $ p (Atom x)
      e2 = evalAtom $ q (Atom x)
      Atom p1 = e1
      Atom p2 = e2
    in 
      p1 `Implies` p2

some :: Typeable n => Show n => Term ((NP n ⇑ S Sem) / N n Sem)
some = LamL $ \(LamN p) -> LamS $ \q -> Atom $ Val $
    Exists $ \(x :: Var n) -> let 
      Atom p1 = eval $ p (Atom x)
      Atom p2 = eval $ q (Atom x)
    in
      p1 `And` p2

no :: Typeable n => Show n => Term ((NP n ⇑ S Sem) / N n Sem)
no = LamL $ \(LamN p) -> LamS $ \q -> Atom $ Val $ 
    Exists $ \(x :: Var n) -> let 
      Atom p1 = eval $ p (Atom x)
      Atom p2 = eval $ q (Atom x)
    in
      Not $ p1 `And` p2

mortal = Atom $ Val Mortal

-- syllogism1 = socrates .> is <. a man

syllogism2 = AppS (every <. man) (\x -> x .> (is <. mortal))

syllogism3 = socrates .> (is <. mortal)

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