
{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies, GADTs, FlexibleInstances #-}

module Montague.Experimental.Typed where

-- An experimental module with a more strongly typed representation of Montague semantics.
-- This also will include things like indexicals and possible world semantics.

import Montague.Experimental.Worlds
import Montague.Experimental.Prop4
import GHC.Types

data LambekType = 
    T Type 
  | L LambekType LambekType
  | R LambekType LambekType

type Sentence = 'T Bool

-- | Type family to get the semantic interpretation of a lambek
-- type.
type family Bracket (x :: LambekType) :: Type where
  Bracket (L a b)  = Bracket a -> Bracket b
  Bracket (R a b)  = Bracket a -> Bracket b
  Bracket (T a) = a

-- Note: If we want to have possible worlds semantics.
-- this is not as general as it could be.
data Term a where
    Lam  :: (Term a -> Term b) -> Term (a -> b)
    App  :: Term (a -> b) -> Term a -> Term b
    Atom :: a -> Term a
    And  :: Term Bool -> Term Bool   -> Term Bool
    Or   :: Term Bool -> Term Bool   -> Term Bool
    All  :: (Term a    -> Term Bool) -> Term Bool
    Some :: (Term Bool -> Term Bool) -> Term Bool

data LambekTerm a where
    LLamL  :: (LambekTerm a -> LambekTerm b) -> LambekTerm (L a b)
    LLamR  :: (LambekTerm a -> LambekTerm b) -> LambekTerm (R a b)
    LAppL  :: LambekTerm a -> LambekTerm (L a b) -> LambekTerm b
    LAppR  :: LambekTerm (R a b) -> LambekTerm a -> LambekTerm b
    LAtom :: a -> LambekTerm (T a)
    LAnd  :: LambekTerm Sentence -> LambekTerm Sentence -> LambekTerm Sentence
    LOr   :: LambekTerm Sentence -> LambekTerm Sentence -> LambekTerm Sentence
    LAll  :: (LambekTerm a -> LambekTerm Sentence) -> LambekTerm Sentence
    LSome :: (LambekTerm Sentence -> LambekTerm Sentence) 
       -> LambekTerm Sentence

instance Eq a => Eq (LambekTerm (T a)) where
  (LAtom x) == (LAtom y) = x == y
  _ == _ = False

-- Example from SEP:
every :: Term ((a -> Bool) -> (a -> Bool) -> Bool)
every = Lam $ \p -> Lam $ \q -> All $ \x ->
  And (App p x) (App q x)

data Person = Nate | Will | Michael | Andrew deriving(Eq)

likes :: LambekTerm (L (T Person) (R (T Person) Sentence))
likes = LLamL $ \x -> LLamR $ \y -> LAtom $
    x == nate && y == william || x == william && y == nate

nate :: LambekTerm (T Person)
nate = LAtom Nate

william :: LambekTerm (T Person)
william = LAtom Will

example :: LambekTerm Sentence
example = nate `LAppL` likes `LAppR` william

-- | -ed morpeme: play-ed -> played. am-ed -> was. see-ed -> saw.
ed :: Term TBool -> Term TBool
ed = undefined

-- | "will" -- a future tense modal operator over
-- 
will :: Term TBool -> Term TBool
will = undefined

-- | Here is what an evaluation function for our algebra could look like.
-- We can try to evaluate universal and existential propositions,
-- and "fail" if they are not definitive.
eval :: Term Bool -> Prop4
eval = undefined