
module Montague.Lexicon where

import Montague.Types
import Montague.Semantics
import Text.Parsec hiding (token, ParseError)
import Text.ParserCombinators.Parsec.Char
import GHC.Real (odd)
import Data.Proxy
import GHC.TypeLits
import Data.PartialOrd hiding ((==))
import Data.Void
import Data.Function
import Data.Constraint

------------- Public API --------------

-- | Load a montague lexicon from a file.
loadLexicon :: FilePath -> IO SomeLexicon
loadLexicon = undefined

-- | Load a montague lexicon from it's contents.
parseLexicon :: String -> Maybe SomeLexicon
parseLexicon x = undefined

------------- Type Parsing ------------

data ParseError =
  -- Type not defined (did you mean...)
    TypeNotDefined (Maybe String)
  -- Type name must be alphanumeric starting with an uppercase letter.
  | InvalidType
  -- The given word was not found in the current lexicon
  | WordNotFound String


data SomeTypeLexicon = forall t. (Show t, Eq t, PartialOrd t) => SomeTypeLexicon {
  typeProxy :: Proxy t,
  parseType :: String -> Maybe t
}

data SomeLexicon = forall a t. (Eq a, Eq t, PartialOrd t, Show a, Show t) => SomeLexicon {
  _someLexicon_typeProxy :: Proxy t,
  entityProxy :: Proxy a,
  semantics :: MontagueSemantics a t (AnnotatedTerm a t)
}

data SomeSymbolList = forall (ss :: [Symbol]). AllKnownSymbols ss => SomeSymbolList (SymbolList ss)

data SymbolList (ss :: [Symbol]) where
    SLNil :: SymbolList '[]
    SLCons :: (KnownSymbol s, AllKnownSymbols ss) => Proxy s -> SymbolList ss -> SymbolList (s ': ss) 

toSymbolList :: [String] -> SomeSymbolList
toSymbolList [] = SomeSymbolList SLNil
toSymbolList (x:xs) = someSymbolVal x & \case
  SomeSymbol sym -> toSymbolList xs & \case
    SomeSymbolList syms -> 
      SomeSymbolList $ SLCons sym syms

-- | Helper function to combine the parser for the head of a
--    ShowableEnum with the parser for the tail of a ShowableEnum
--    to produce a parser for the whole ShowableEnum.
combineTypeParsers :: (KnownSymbol x, AllKnownSymbols xs) => (String -> Maybe (ShowableEnum '[x])) 
  -> (String -> Maybe (ShowableEnum xs)) 
  -> (String -> Maybe (ShowableEnum (x ': xs)))
combineTypeParsers xParser xsParser = \input ->
    case xParser input of
        Just res -> pure $ injectHead res
        Nothing  -> case xsParser input of
            Just res -> pure $ injectTail res
            Nothing  -> Nothing

injectHead :: forall x xs. (KnownSymbol x, AllKnownSymbols xs) => ShowableEnum '[x] -> ShowableEnum (x ': xs)
injectHead (SEInl sym _) = SEInl sym (Proxy @xs)

injectTail :: forall x xs. (KnownSymbol x, AllKnownSymbols xs) => ShowableEnum xs -> ShowableEnum (x ': xs)
injectTail e = SEInr (Proxy @x) e

parseTypeLexicon' :: AllKnownSymbols ss => 
       SymbolList ss 
    -> (String -> Maybe (ShowableEnum ss))
parseTypeLexicon' (SLCons sym rest) = 
    let 
        headParser = \i -> if i == (symbolVal sym)
          then Just $ SEInl sym Proxy
          else Nothing
        tailParser = parseTypeLexicon' rest
    in
        combineTypeParsers
            headParser
            tailParser

parseTypeLexicon :: [String] -> SomeTypeLexicon
parseTypeLexicon types = types & toSymbolList & \case
    (SomeSymbolList syms) -> SomeTypeLexicon Proxy $ \i ->
        parseTypeLexicon' syms i

type EntityDeclarations =
    [(String, String)]

type ProductionDeclarations =
    [([String], String)] 

parseSomeLexicon :: SomeTypeLexicon 
    -> EntityDeclarations 
    -> ProductionDeclarations
    -> Either ParseError SomeLexicon
parseSomeLexicon (SomeTypeLexicon typeProxy lex) entityDecls productions = 
    let entities = entityDecls & 
          fmap fst &
          toSymbolList
    in
      pure $ entities & \case
        SomeSymbolList syms ->
            let 
              typeOf = parseTypeOf entityDecls
              parseTerm = parseParseTerm productions
              semantics = MontagueSemantics 
                  typeOf parseTerm id
            in 
              SomeLexicon (getEnumType syms) 
                 typeProxy 
                 semantics

getEnumType :: SymbolList ss -> Proxy (ShowableEnum ss)
getEnumType _ = Proxy

parseTypeOf :: 
     (Eq a, Parsable t)
  => EntityDeclarations 
  -> (a -> MontagueType t)
parseTypeOf decls = let
    pairs = decls & 
      map (\(x, y) -> (parse @a, parse @t))
  in \entity ->
      snd <$> first (\(x, y) -> entity == x) pairs

parseParseTerm :: 
     (Parsable a, Parsable t)
  => ProductionDeclarations 
  -> (String -> MontagueTerm a t)
parseParseTerm decls = let
    pairs = join $ (\(xs, y) -> (\x -> (x, parse @a y)) <$> xs) <$> decls
  in \input ->
    Atom <$> snd <$> first (\(x, y) -> input == x) pairs

data ShowableType (s :: Symbol) = ShowableType (Proxy s)

instance KnownSymbol s => Show (ShowableType s) where
    show (ShowableType p) = symbolVal p 

-- | Alternative Either implementaton with
-- custom Show instance (does not display tags).
data Sum x y = 
      Sum1 x
    | Sum2 y

instance (Show x, Show y) => Show (Sum x y) where
    show (Sum1 x) = show x
    show (Sum2 y) = show y

data ShowableEnum (ss :: [Symbol]) where
    SEInl  :: (KnownSymbol s, AllKnownSymbols ss) => Proxy s -> Proxy ss -> ShowableEnum (s ': ss)
    SEInr  :: (KnownSymbol s, AllKnownSymbols ss) => Proxy s -> ShowableEnum ss -> ShowableEnum (s ': ss)

-- | Helper function for types parsable from a string.
class Parsable a where
  parse :: String -> Maybe a 

instance AllKnownSymbols ss => Show (ShowableEnum ss) where
    show (SEInl p _)    = symbolVal p
    show (SEInr _ rest) = show rest

instance AllKnownSymbols ss => PartialOrd (ShowableEnum ss) where
    (SEInl sym1 _) <= (SEInl sym2 _)       = symbolVal sym1 == symbolVal sym2
    (SEInr p1 rest1) <= (SEInr p2 rest2) = rest1 Data.PartialOrd.<= rest2
    _             <= _                   = False 

instance AllKnownSymbols ss => Eq (ShowableEnum ss) where
    (SEInl sym1 _)   == (SEInl sym2 _)   = symbolVal sym1 == symbolVal sym2
    (SEInr p1 rest1) == (SEInr p2 rest2) = rest1 == rest2
    _                == _                = False 

instance AllKnownSymbols ss => Parsable (ShowableEnum ss) where
  parse input = let 
      values = zip (enumValues' (Proxy @ss)) 
        (symbolVals (Proxy @ss))
    in fst <$> find (\x -> snd x == input) values

enumValues :: SymbolList ss -> [ShowableEnum ss]
enumValues x = case x of
  SLNil -> [] 
  SLCons sym syms -> 
    let values  = enumValues syms
        lastSym = head values 
    in [SEInl sym Proxy] ++ (SEInr sym <$> values)

enumValues' :: AllKnownSymbols ss => Proxy ss -> [ShowableEnum ss]
enumValues' x = enumValues $ symbolList x

class AllKnownSymbols (ss :: [Symbol]) where
  symbolVals :: Proxy ss -> [String]
  symbolList :: Proxy ss -> SymbolList ss

instance AllKnownSymbols '[] where
  symbolVals _ = []
  symbolList _ = SLNil

instance forall s ss. (KnownSymbol s, AllKnownSymbols ss) => AllKnownSymbols (s ': ss) where
  symbolVals Proxy = symbolVal (Proxy @s) : symbolVals (Proxy @ss)
  symbolList _     = SLCons (Proxy @s) (symbolList (Proxy @ss))

data SomeShowableEnum = forall (ss :: [Symbol]). 
    SomeShowableEnum (ShowableEnum ss)

------------- Parsers --------------

comment = do
    char '%'
    many (char ' ')
    char '\n'

token :: Parsec String () t -> Parsec String () ()
token p = do
    many (char ' ' <|> char '\n' <|> comment)
    _ <- p
    many (char ' ' <|> char '\n' <|> comment)
    return ()

orT       = token (char '|')
equals    = token (char '=')
end       = token (char '.')
subtypeOf = token (string ":<") 
typeOfT   = token (char ':') 
comma     = token (char ',')
arrow     = token (string "-->")
typeToken = token (string "Type")

entityT = do
    lower
    many alphaNum

textT = many alphaNum

typeDeclaration = do
    typeToken
    equals
    sepBy1 entityT orT

subtypeDeclaration = do
    x <- entityT
    subtypeOf
    y <- entityT
    return (x, y)

atomDeclaration = do
    x <- entityT
    typeOfT
    y <- entityT
    return (x, y)

productionDeclaration = do
    x <- sepBy1 textT comma
    arrow
    y <- entityT
    return (x, y)

montagueLexicon = do
    types <- typeDeclaration
    let typeLexicon = parseTypeLexicon types

    -- Ignore this for now. This will be used
    --  when we implement subtyping.
    _ <- many subtypeDeclaration

    atoms <- many atomDeclaration
    productions <- many productionDeclaration

    -- TODO: Add better error handling here.
    let Right lexicon = parseSomeLexicon typeLexicon atoms productions

    return $ lexicon