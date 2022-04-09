{-# LANGUAGE LambdaCase, ScopedTypeVariables, TypeApplications, TypeOperators, DataKinds, GADTs, KindSignatures #-}

module Montague.Lexicon where

import Montague.Types
import Montague.Semantics
import Control.Applicative (empty)
import Text.Parsec hiding (token, parse, ParseError)
import qualified Text.Parsec (parse)
import Text.ParserCombinators.Parsec.Char
import GHC.Real (odd)
import Data.Proxy
import GHC.TypeLits
import Data.PartialOrd hiding ((==))
import Control.Monad
import Data.Void
import Data.Function
import Data.Maybe
import Data.List

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


data SomeTypeLexicon = forall t. (Bounded t, Enum t, Show t, Eq t, PartialOrd t, Parsable t) => SomeTypeLexicon {
  typeProxy :: Proxy t,
  parseType :: String -> Maybe t
}

data SomeLexicon = forall a t. (Bounded a, Bounded t, Enum a, Enum t, Eq a, Eq t, Parsable t, Parsable a, PartialOrd t, Show a, Show t) => SomeLexicon {
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
              entityProxy = getEnumType syms
              typeOf = parseTypeOf typeProxy entityDecls
              parseTerm = parseParseTerm typeProxy productions
              semantics = MontagueSemantics
                  typeOf parseTerm id
            in
              SomeLexicon typeProxy
                 entityProxy
                 semantics

getEnumType :: SymbolList ss -> Proxy (ShowableEnum ss)
getEnumType _ = Proxy

parseTypeOf :: forall a t.
     (Eq a, Parsable a, Parsable t)
  => Proxy t
  -> EntityDeclarations
  -> (a -> MontagueType t)
parseTypeOf _ decls = let
    pairs = decls &
      map (\(x, y) -> (fromJust $ parse @a x, fromJust $ parse @t y))
  in \entity -> maybe empty pure $ BasicType <$>
      snd <$> find (\(x, y) -> entity == x) pairs

parseParseTerm :: forall a t.
     (Parsable a, Parsable t)
  => Proxy t
  -> ProductionDeclarations
  -> (String -> MontagueTerm a t)
parseParseTerm _ decls = let
    pairs = join $ (\(xs, y) -> (\x -> (x, fromJust $ parse @a y)) <$> xs) <$> decls
  in \input ->
    maybe empty pure $
        Atom <$> snd <$>
           find (\(x, y) -> input == x) pairs

data ShowableEnum (ss :: [Symbol]) where
    SEInl  :: (KnownSymbol s, AllKnownSymbols ss) => Proxy s -> Proxy ss -> ShowableEnum (s ': ss)
    SEInr  :: (KnownSymbol s, AllKnownSymbols ss) => Proxy s -> ShowableEnum ss -> ShowableEnum (s ': ss)

instance AllKnownSymbols ss => Enum (ShowableEnum ss) where
    fromEnum x = fromJust $ elemIndex x (enumValues' Proxy)  
    toEnum   n = enumValues' Proxy !! n

instance AllKnownSymbols ss => Bounded (ShowableEnum ss) where
    minBound = head $ enumValues' Proxy
    maxBound = last $ enumValues' Proxy

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

parseSchema input = Text.Parsec.parse
    (montagueLexicon  <* eof) "" input

comment = do
    char '%'
    many (char ' ')
    char '\n'

token :: Parsec String () t -> Parsec String () t
token p = do
    many (char ' ' <|> char '\n' <|> comment)
    res <- p
    many (char ' ' <|> char '\n' <|> comment)
    return res

orT       = token (char '|')
equals    = token (char '=')
end       = token (char '.')
subtypeOf = token (string ":<")
typeOfT   = token (char ':')
comma     = token (char ',')
arrow     = token (string "-->")
typeToken = token (string "Type")

entityT = token $ do
    x <- lower
    xs <- many alphaNum
    pure (x:xs)

textT = token $ many alphaNum

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
    end
    let typeLexicon = parseTypeLexicon types

    -- Ignore this for now. This will be used
    --  when we implement subtyping.
    -- _ <- many subtypeDeclaration

    atoms <- many atomDeclaration
    end
    productions <- many productionDeclaration
    end
    -- TODO: Add better error handling here.
    let Right lexicon = parseSomeLexicon typeLexicon atoms productions

    return $ lexicon