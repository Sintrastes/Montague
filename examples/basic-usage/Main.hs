
import Control.Monad.Tree
import Montague
import Montague.Types

-- An example basic type to use with a schema.
data BasicType =
   Sentence
 | Question BasicType
 | Noun
 | Person
 | ProperNoun
 | Determiner
 | DeterminedNoun
 | UndeterminedNoun deriving(Eq, Show)

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
   | Mom
   | Mother
   | Dad
   | Father
   | Dog
   | Cat
   | With
   | Spoon
   | Is

typeOf :: Term BasicAtom BasicType -> MontagueType BasicType
typeOf = undefined

adjective :: Tree () LambekType
adjective = noun `leftArrow` noun
          <|> noun

noun :: Tree () LambekType
noun = return $ BasicType Noun

sentence :: Tree () LambekType
sentence = return $ BasicType Sentence

verb :: Tree () LambekType
verb = noun `rightArrow` (sentence `leftArrow` noun)
     <|> (noun `rightArrow` sentence) `leftArrow` noun

-- Example lexicon.
myLexicon :: Lexicon BasicType
myLexicon = \case
   "reading" ->   (sentence `leftArrow` noun) `rightArrow` (sentence `leftArrow` noun)
             <|> (sentence `leftArrow` noun) `rightArrow` sentence
   "lazy"    -> adjective
   "i"       -> noun
   "me"      -> noun
   "he"      -> noun
   "she"     -> noun
   "it"      -> noun
   "killed"  -> noun `rightArrow` (sentence `leftArrow` noun)
            <|> (noun `rightArrow` sentence) `leftArrow` noun
   "the"     -> noun `leftArrow` noun
   "super"   -> adjective `leftArrow` adjective
            <|> adjective
   "hello"   -> sentence
   "hi"      -> sentence
   "bye"     -> sentence
   "my"      -> noun `leftArrow` noun
   "name"    -> noun
   "is"      -> verb
   "am"      -> verb
   "are"     -> verb
   "bob"     -> noun
   "mary"    -> noun
   "moses"   -> noun
   "nate"    -> noun
   "rick"    -> noun
   "rachel"  -> noun
   "william" -> noun
   "michael" -> noun
   "book" -> noun
   "table" -> noun
   "chair" -> noun
   "rock" -> noun
   "tree" -> noun
   "and"  -> sentence `rightArrow` (sentence `leftArrow` sentence)
   "or"   -> sentence `rightArrow` (sentence `leftArrow` sentence)
   "happy" -> adjective <|> noun
   "green" -> adjective
   "sad"    -> noun
   "mad"    -> noun
   "joyous" -> noun
   "angry"  -> noun
   "happily" -> sentence `rightArrow` sentence
                    <|> verb `leftArrow` verb
   "quickly" -> sentence `rightArrow` sentence
                    <|> verb `leftArrow` verb
   "slowly"  -> sentence `rightArrow` sentence
                    <|> verb `leftArrow` verb
   "carefully" -> sentence `rightArrow` sentence
                       <|> verb `leftArrow` verb
   "man"     -> noun
   "woman"   -> noun
   "mom"     -> noun
   "mother"  -> noun
   "dad"     -> noun
   "father"  -> noun
   "cat"     -> noun
   "dog"     -> noun
   "with"    -> noun `rightArrow` (noun `leftArrow` noun)
                    <|> (sentence `rightArrow` sentence) `leftArrow` noun
   "when" -> (sentence `rightArrow` sentence) `leftArrow` sentence
   "that"    -> noun `rightArrow` ((noun `leftArrow` (sentence `leftArrow` noun)) 
                    <|> (noun `leftArrow` (noun `rightArrow` sentence)))
   "who"     -> noun `rightArrow` (noun `leftArrow` (sentence `leftArrow` noun))
   "a"       -> noun `leftArrow` noun
   "spoon"   -> noun
   _ -> Node () []

main = do
   putStrLn "Test"
