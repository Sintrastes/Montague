
import Prelude hiding ((<=))
import Control.Applicative
import Control.Monad.Tree
import Montague
import Montague.Types
import Data.Proxy
import Montague.Semantics
import Data.PartialOrd hiding ((==))
import System.Environment

-- An example basic type to use with a schema.
data BasicType =
   Sentence
 | Question BasicType
 | Noun
 | Adjective
 | Person
 | Thing
 | ProperNoun
 | Determiner
 | DeterminedNoun
 | UndeterminedNoun deriving(Eq, Show)

instance PartialOrd BasicType where
    Person <= Noun = True
    Thing  <= Noun = True
    x <= y | x == y = True
    _ <= _ = False

data BasicAtom =
     Nate
   | Rick
   | Rachel
   | Will
   | Michael
   | Book
   | Table
   | Chair
   | Happy
   | Green
   | Sad
   | Mad
   | Mother
   | Father
   | Dog
   | Cat
   | With
   | Spoon
   | Is deriving(Show, Eq)

typeOfTerm :: Term BasicAtom BasicType -> MontagueType BasicType
typeOfTerm = \case
    Atom Nate    -> pure $ BasicType Person
    Atom Rick    -> pure $ BasicType Person
    Atom Rachel  -> pure $ BasicType Person
    Atom Will    -> pure $ BasicType Person
    Atom Michael -> pure $ BasicType Person
    Atom Book    -> pure $ BasicType Thing
    Atom Table   -> pure $ BasicType Thing
    Atom Chair   -> pure $ BasicType Thing
    Atom Is      -> pure $ ((BasicType Noun) `RightArrow` (BasicType Sentence)) `LeftArrow` (BasicType Adjective)
    Atom Happy   -> pure $ BasicType Adjective
    App (Atom Happy) [x] 
         -> pure $ BasicType Sentence
--    Atom Sad     ->
--    Atom Mad     ->
    Atom Mother  -> pure $ BasicType Person
    Atom Father  -> pure $ BasicType Person
    Atom Dog     -> pure $ BasicType Person
    Atom Cat     -> pure $ BasicType Person
-- Atom With
    Atom Spoon   -> pure $ BasicType Thing

-- Example lexicon.
myLexicon :: Lexicon BasicAtom BasicType
myLexicon = \case
    "nate"    -> pure $ Atom Nate
    "nathan"  -> pure $ Atom Nate
    "rick"    -> pure $ Atom Rick
    "rachel"  -> pure $ Atom Rachel
    "will"    -> pure $ Atom Will
    "william" -> pure $ Atom Will
    "michael" -> pure $ Atom Michael
    "book"    -> pure $ Atom Book
    "table"   -> pure $ Atom Table
    "chair"   -> pure $ Atom Chair
    "mother"  -> pure $ Atom Mother
    "mom"     -> pure $ Atom Mother
    "father"  -> pure $ Atom Father
    "dad"     -> pure $ Atom Father
    "cat"     -> pure $ Atom Cat
    "dog"     -> pure $ Atom Dog
    "spoon"   -> pure $ Atom Spoon
    "is"      -> pure $ Atom Is
    "happy"   -> pure $ Atom Happy
    _         -> empty

instance MontagueSemantics BasicAtom BasicType (AnnotatedTerm BasicAtom BasicType) where
   typeOf    = typeOfTerm
   parseTerm = myLexicon
   interp    = id


adjective = noun `leftArrow` noun
          <|> noun

noun = return $ BasicType Noun

sentence = return $ BasicType Sentence

verb = noun `rightArrow` (sentence `leftArrow` noun)
     <|> (noun `rightArrow` sentence) `leftArrow` noun

main = do
   args <- getArgs
   let input = args !! 0
   case getParse (Proxy @BasicAtom) input of
      Nothing     -> putStrLn "Failed to parse input."
      Just parsed -> print parsed
