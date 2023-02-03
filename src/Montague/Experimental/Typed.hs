
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

instance (Typeable _Ω, Show _Ω) => BooleanType _Ω (S _Ω) where
  coord f (Sentence x) (Sentence y) = Sentence $ Val $ f x y

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
    LamN  :: (Term (NP Nothing ind) -> Term (S o)) -> Term (N ind o)
    AppL  :: Term (b / a) -> Term a -> Term b
    AppR  :: Term a -> Term (a \\ b) -> Term b
    -- | Constructor for applying extracted functions.
    --
    -- Note: NOT a valid rule in the calculus, only used
    -- internally.
    AppE  :: Term (a ↑ b) -> Term b -> Term a
    -- Also not sure if this is right or not.
    AppS  :: Term (b ⇑ a) -> (Term b -> Term a) -> Term a
    Atom  :: (Show a, Typeable a) => Var a -> Term (NP 'Nothing a)
    TypedAtom :: (Show a, Typeable a) => Var a -> Term (NP t a)
    -- | Time, in terms of hours and minutes.
    TimeAtom :: Int -> Int -> Term Time
    EvSentence :: (Show sem, Typeable sem) => (Term ExIt -> Var sem) -> Term (EvS sem)
    Sentence  :: (Show sem, Typeable sem) => Var sem -> Term (S sem)
    Interrogative :: (Show sem, Typeable sem) => Term (S sem) -> Term (Sy sem)
    WhSentence :: (Term (NP Nothing a) -> Term (S sem)) -> Term (Sw a sem)
    IntroPred :: (Term (NP Nothing n) -> Term (S sem)) -> Term (Pr n sem)
    It :: Term ExIt
    There :: Term ExThere
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

argument :: Term (NP t a) -> Term (NP Nothing a)
argument (TypedAtom x) = Atom x
argument (Atom x) = Atom x

-- evalAtom :: Term a -> Term a
evalSentence :: (Show sem, Typeable sem) => Term (S sem) -> Term (S sem)
evalSentence x = evalSentence' $ eval x

evalSentence' (TVar x) = Sentence $ Var x
evalSentence' x = x

evalAtom :: (Show a, Typeable a) => Term (NP Nothing a) -> Term (NP Nothing a)
evalAtom x = evalAtom' $ eval x

evalAtom' (TVar x) = Atom $ Var x
evalAtom' x = x

evalTypedAtom :: (Show a, Typeable a) => Term (NP t a) -> Term (NP t a)
evalTypedAtom x = evalTypedAtom' $ eval x

evalTypedAtom' (TVar x) = TypedAtom $ Var x
evalTypedAtom' x = x

(<.) = AppL
(.>) = AppR

-- | An example type of raw semantic expressions.
data Sem where
    Pred       :: String -> [Sem] -> Sem
    Individual :: Show a => Var a -> Sem
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
    TypedAtom x -> show x
    TimeAtom h m -> show h ++ ":" ++ show m
    Sentence x -> show x
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

instance Eq a => Eq (Term (NP t a)) where
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
    | Robin
    | Bob
    | Socrates 
  deriving(Eq, Show, Enum)

data Object = 
    PictureA
  | PictureB
  | PictureC
  | Rock
  | Camera
  | Ball
  | Tree
  | MountainA
  | MountainB
  | MountainC
  | River
  | Stream
     deriving(Show, Eq, Enum, Bounded)

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
likes :: Term ((NP Nothing Person \\ S Sem) / NP Nothing Person)
likes = LamL $ \(Atom x) -> LamR $ \(Atom y) -> Sentence $ Val $
    Pred "likes" [Individual x, Individual y]

-- | Likes, in the sense of a person and a subject.
likes2 :: Term ((NP Nothing Person \\ S Sem) / NP Nothing Subject)
likes2 = LamL $ \(Atom x) -> LamR $ \(Atom y) -> Sentence $ Val $
    Pred "likes" [Individual y, Subject x]

-- | Definite determiner -- converts a noun into a
-- noun phrase.
the :: forall a. 
      Typeable a 
   => Show a 
   => Enum a 
   => Eval Sem Bool 
   => Term (N a Sem) -> Term (NP Nothing a)
the p' = let 
  LamN p = eval p' 

  entities :: [a] = [toEnum 0 ..]
  holdsFor = filter (evalAtom . p . Atom . Val) entities

  evalAtom :: Term (S Sem) -> Bool
  evalAtom (Sentence (Val x)) = evaluate x
  evalAtom (Sentence (Var _)) = False
 in 
  Atom (Val $ trace (show holdsFor) $ head holdsFor)

-- | "did", as in a question.
did :: Term (Sy Sem / (NP Nothing a \\ S Sem) / NP Nothing a)
did = LamL $ \x -> LamL $ \v -> let 
  z = AppR x v 
 in 
  Interrogative z

-- | "who", as used in a question.
who :: Term (Sw a Sem / (NP Nothing a \\ S Sem))
who = LamL $ \v -> 
  WhSentence $ \x ->
    AppR x v

-- | "who" or "whom" as used in a question.
whom :: Term (Sw a Sem / (Sy Sem ↑ NP Nothing a))
whom = LamL $ \v ->
  WhSentence $ \x -> let 
    Interrogative z = AppE v x 
  in 
    z

appN :: Term (N a Sem) -> Term (NP Nothing a) -> Term (S Sem)
appN n np = let 
-- Note: This assumes there's no other way to specify a noun.
  LamN n' = n
 in 
  n' np


-- | "which", as used in a question.
which1 :: Term (Sw a Sem / (NP Nothing a \\ S Sem) / N a Sem)
which1 = LamL $ \p -> LamL $ \v -> WhSentence $ \x -> let
  Sentence z1 = evalSentence $ appN p x
  Sentence z2 = evalSentence $ AppR x v
 in
  Sentence $ Val $ And z2 z1

-- | "which", as used in a question.
which2 :: Term (Sw a Sem / (Sy Sem ↑ NP Nothing a) / N a Sem)
which2 = LamL $ \p -> LamL $ \v -> WhSentence $ \x -> let
  Sentence z1 = evalSentence $ appN p x
  Interrogative z2 = AppE v x
  Sentence z3 = evalSentence z2
 in
  Sentence $ Val $ And z3 z1

picture :: Term (N Object Sem / NP (Just Of) Person)
picture = LamL $ \person -> LamN $ \picture -> let 
  Atom picture' = evalAtom picture 
  TypedAtom person' = evalTypedAtom person
 in
  Sentence $ Val $ 
    -- Note: This is a slightly different semantics than in Carpenter.
    --  This is a more neo-davidsonian representation where the argument
    --  "of" is not required.
    And (Val $ Pred "picture" [Individual picture']) 
        (Val $ Pred "of" [Individual picture', Individual person'])

of' :: Term (NP (Just Of) Person / NP Nothing Person)
of' = LamL $ \x -> let
  Atom x' = evalAtom x
 in
   TypedAtom x'

robin :: Term (NP Nothing Person)
robin = Atom $ Val Robin

-- | Carpenter page 135 example
--
--     > eval relationalNounExample
--     λx.picture(x) ∧ of(x, Robin)
-- 
relationalNounExample :: Term (N Object Sem)
relationalNounExample = picture <. (of' <. robin)

-- | An example of a noun which is not a proper noun.
man :: Term (N Person Sem)
man = LamN $ \x -> let Atom x' = eval x in
  Sentence $ Val $ Pred (show Man) [Individual x']

-- who (or which, since we do not distinguish animacy), used to construct relative
--  clauses.
who2 :: Term (N n Sem \\ N n Sem / (S Sem ↑ NP Nothing n))
who2 = LamL $ \v -> LamR $ \p -> LamN $ \x -> let 
     Sentence a1 = v `AppE` x
     LamN p' = p
     Sentence a2 = p' x
  in
     Sentence $ Val $ And a1 a2

relativeClauseExample :: Term (S Sem ↑ NP Nothing Person)
relativeClauseExample = LamE $ \x ->
    x .> (likes <. william)

-- "the man who likes William"
relativeClauseUsage :: Term (NP Nothing Person)
relativeClauseUsage = the $ man .> (who2 <. LamE (\x -> x .> (likes <. william)))

-- "nate likes william and michael"
rightCoordinationExample = (nate .> assoc likes) 
  .> (raise' william `and'` raise' michael)

-- "the man who likes cooking and anime"
relativeClauseUsage2 = the $ 
  man .> (who2 <. 
    LamE (\x -> (x .> assoc likes2) 
        .> (raise' cooking `and'` raise' anime)))

it :: Term ExIt
it = It

there :: Term ExThere
there = There

rained :: Term (ExIt \\ S Sem)
rained = LamR $ \_ -> Sentence $
  Val $ Pred "rained" []

-- | An example of a spoken time.
twoThirty :: Term Time
twoThirty = TimeAtom 2 30

-- | Is, in terms of being a particular time.
isT :: Term ((ExIt \\ S Sem) / Time)
isT = LamL $ \(TimeAtom h m) -> 
  LamR $ \_ ->
    Sentence $ Val $ 
      Pred "time_is" [Individual (Val h), Individual (Val m)]

timeExample = it .> (isT <. twoThirty)

-- | Neo-davidsonian variant of "rained" that says something about
-- weather events.
--
-- Except I'm not sure how this would compose with adjectives.
--
-- I guess the easiest way might be to introduce a new category
--  "EvS" for sentences abstracted over events, and give a rule
--  EvS -> S that existentially quanitifes over the event.
--
rainedD :: Term (ExIt \\ EvS Sem)
rainedD = LamR $ \_ -> EvSentence $ \e -> 
  Val $ Pred "rained" [Individual $ Val e]
 
snowed :: Term (ExIt \\ S Sem)
snowed  = LamR $ \_ -> Sentence $
  Val $ Pred "snowed" []

-- | Following carpenter
weatherExample = it .> snowed

weatherExample2 = it .> (rainedD .> heavily)

-- Example of an adverb specifically acting on weather predicates.
heavily :: Term ((ExIt \\ EvS Sem) \\ (ExIt \\ EvS Sem))
heavily = LamR $ \p -> LamR $ \_ -> 
  EvSentence $ \e -> let
   EvSentence z = AppR e p  
  in
   Val $ And 
     (z e)
     (Val $ Pred "heavily" [Individual $ Val e])

almost :: Term ((ExIt \\ S Sem) / (ExIt \\ S Sem))
almost = LamL $ \p -> LamR $ \e -> 
  AppR e p

nate :: Term (NP Nothing Person)
nate = Atom $ Val Nate

william :: Term (NP Nothing Person)
william = Atom $ Val William

michael = Atom $ Val Michael

socrates = Atom $ Val Socrates

class Entity n o | n -> o where
  inj :: Var n -> o

instance Entity Person Sem where
  inj = Individual

-- | The copula, in the sense of attribution.
is :: Typeable n => Show n => Show a => Entity n Sem => HasAttributes n a => Term (NP Nothing n \\ S Sem / NP Nothing a)
is = LamL $ \attr -> LamR $ \e -> let Atom entity = eval e in
  Sentence $ Val $ Pred (show attr) [inj entity]

-- | The copula, in alternate formulation involving predicative categories.
-- 
-- Note that this still allows us to restrict predicates,
--  as Pr is indexed over the type of noun.
--
-- It just means that this has be enforced at the level of the 
--  underlying semantic category.
--
isPr :: Term (NP Nothing n \\ S Sem / Pr n Sem)
isPr = LamL $ \(IntroPred attr) -> LamR $ \e -> 
  attr e

every :: Typeable n => Show n => Term ((NP Nothing n ⇑ S Sem) / N n Sem)
every = LamL $ \p' -> LamS $ \q -> Sentence $ Val $ 
    All $ \x -> let 
      LamN p = p'
      e1 = eval $ p (Atom x)
      e2 = eval $ q (Atom x)
      Sentence p1 = e1
      Sentence p2 = e2
    in 
      p1 `Implies` p2

some :: Typeable n => Show n => Term ((NP Nothing n ⇑ S Sem) / N n Sem)
some = LamL $ \(LamN p) -> LamS $ \q -> Sentence $ Val $
    Exists $ \(x :: Var n) -> let 
      Sentence p1 = eval $ p (Atom x)
      Sentence p2 = eval $ q (Atom x)
    in
      p1 `And` p2

no :: Typeable n => Show n => Term ((NP Nothing n ⇑ S Sem) / N n Sem)
no = LamL $ \(LamN p) -> LamS $ \q -> Sentence $ Val $ 
    Exists $ \(x :: Var n) -> let 
      Sentence p1 = eval $ p (Atom x)
      Sentence p2 = eval $ q (Atom x)
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

-- Example of a question
example4 = who <. (likes <. william)

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