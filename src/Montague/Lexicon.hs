{-# LANGUAGE LambdaCase, PartialTypeSignatures, ScopedTypeVariables, TypeApplications, TypeOperators, DataKinds, GADTs, KindSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Montague.Lexicon where

import Montague.Types
import Montague.Semantics
import Text.Parsec
    ( alphaNum,
      char,
      lower,
      string,
      upper,
      between,
      chainl1,
      eof,
      sepBy1,
      Parsec,
      ParsecT, try, manyTill, sepBy )
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
import Data.Functor.Identity
import Control.Monad
import Control.Applicative

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

instance Show ParseError where
    show = \case
        TypeNotDefined ms -> case ms of
            Nothing -> "Type not defined."
            (Just s) -> "Type not defined. Did you mean " <> s <> "?"
        InvalidType -> "Type name must be alphanumeric starting with an uppercase letter."
        WordNotFound s -> "The word " <> s <> " was not found in the current lexicon."


data SomeTypeLexicon = forall t. (Bounded t, Enum t, Show t, Eq t, PartialOrd t, Parsable t) => SomeTypeLexicon {
  typeProxy :: Proxy t,
  parseType :: String -> Maybe t
}

data SomeLexicon = forall a t. (Bounded a, Bounded t, Enum a, Enum t, Eq a, Eq t, Parsable t, Parsable a, PartialOrd t, Show a, Show t) => SomeLexicon {
  _someLexicon_typeProxy :: Proxy t,
  entityProxy :: Proxy a,
  getDocs :: a -> String,
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
combineTypeParsers xParser xsParser input =
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

type EntityDeclarations t =
    [DocumentedEntity String t]

type ProductionDeclarations =
    [([String], String)]

parseSomeLexicon :: _ => (String -> Maybe t)
    -> EntityDeclarations t
    -> ProductionDeclarations
    -> Either ParseError SomeLexicon
parseSomeLexicon lex entityDecls productions =
    let entities = entityDecls &
          fmap entity &
          toSymbolList
    in
      pure $ entities & \case
        SomeSymbolList syms ->
            let
              entityProxy = getEnumType syms
              typeOf = parseTypeOf Proxy entityDecls
              parseTerm = parseParseTerm Proxy productions
              semantics = MontagueSemantics
                  typeOf parseTerm id
            in
              SomeLexicon Proxy
                 entityProxy
                 (\ent -> maybe "" entityDocs $ 
                    find (\x -> entity x == show ent) entityDecls)
                 semantics

getEnumType :: SymbolList ss -> Proxy (ShowableEnum ss)
getEnumType _ = Proxy

parseTypeOf :: forall a t.
     (Eq a, Parsable a, Parsable t)
  => Proxy t
  -> EntityDeclarations t
  -> (a -> MontagueType t)
parseTypeOf _ decls = let
    pairs = decls &
      map (\(DocumentedEntity _ x y) -> (fromJust $ parse @a x, y))
  in \entity -> maybe empty snd
        (find (\(x, y) -> entity == x) pairs)

parseParseTerm :: forall a t.
     (Parsable a, Parsable t)
  => Proxy t
  -> ProductionDeclarations
  -> (String -> MontagueTerm a t)
parseParseTerm _ decls = let
    pairs = (\(xs, y) -> (\x -> (x, fromJust $ parse @a y)) <$> xs) =<< decls
  in \input ->
    maybe empty pure $
        Atom . snd <$>
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
    in SEInl sym Proxy : (SEInr sym <$> values)

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

parseSchema = Text.Parsec.parse
    (montagueLexicon  <* eof) ""

comment = do
    token $ string "--"
    manyTill anyChar (try $ char '\n')
    char '\n'

docString = do
    token $ string "--"
    token $ char '|'
    manyTill anyChar (try $ char '\n')

token :: Parsec String () t -> Parsec String () t
token p = do
    many (try (char ' ' >> pure ()) <|> (char '\n' >> pure ()))
    res <- p
    many (try (char ' ' >> pure ()) <|> (char '\n' >> pure ()))
    return res

orT       = token (char '|')
equals    = token (char '=')
end       = token (char '.')
subtypeOf = token (string ":<")
typeOfT   = token (char ':')
comma     = token (char ',')
arrow     = token (string "-->")
rarrow    = try (token (string "->")) <|> token (string "\\")
larrow    = try (token (string "<-")) <|> token (string "/")
typeToken = token (string "Type")

entityT :: Parsec String () [Char]
entityT = token $ do
    x <- lower
    xs <- many alphaNum
    pure (x:xs)

data DocumentedEntity a t = DocumentedEntity {
  entityDocs :: String,
  entity :: a,
  entityType :: MontagueType t
}

typeIdentT :: Parsec String () [Char]
typeIdentT = token $ do
    x <- upper
    xs <- many alphaNum
    pure (x:xs)

textT :: Parsec String () [Char]
textT = token $ many alphaNum

parens = between (token $ char '(') (token $ char ')')

typesDeclaration :: ParsecT String () Identity [[Char]]
typesDeclaration = do
    typeToken
    equals
    sepBy1 typeIdentT orT

typeDeclaration :: (String -> Maybe t) -> ParsecT String () Identity (MontagueType t)
typeDeclaration parse = atomicTypeExpr parse <|> parens (typeExpr parse)

typeExpr :: (String -> Maybe t) -> ParsecT String () Identity (MontagueType t)
typeExpr parse = chainl1 (typeDeclaration parse) typeOperator

typeOperator :: ParsecT String () Identity (MontagueType t -> MontagueType t -> MontagueType t)
typeOperator =
  (leftArrow  <$ larrow) <|>
  (rightArrow <$ rarrow) <|>
  ((<|>) <$ orT)

atomicTypeExpr :: (String -> Maybe t) -> ParsecT String () Identity (MontagueType t)
atomicTypeExpr parse = do
    id <- typeIdentT
    case parse id of
        Nothing -> fail $ "Could not parse " <> id <> " as a type."
        Just t  -> pure $ pure $ BasicType t

subtypeDeclaration :: ParsecT String () Identity (String, String)
subtypeDeclaration = do
    x <- typeIdentT
    subtypeOf
    y <- typeIdentT
    return (x, y)

atomDeclaration :: (String -> Maybe t) ->  ParsecT String () Identity (String, MontagueType t)
atomDeclaration parse = do
    x <- entityT
    typeOfT
    y <- typeExpr parse
    return (x, y)

documentedEntity :: (String -> Maybe t) -> Parsec String () (DocumentedEntity String t)
documentedEntity parse = do
    docs <- docString
    (entity, typ) <- atomDeclaration parse
    end
    pure $ DocumentedEntity docs entity typ

productionDeclaration :: ParsecT String () Identity ([String], String)
productionDeclaration = do
    x <- sepBy1 textT comma
    arrow
    y <- entityT
    end
    return (x, y)

montagueLexicon :: ParsecT String () Identity SomeLexicon
montagueLexicon = do
    types <- typesDeclaration
    end
    let typeLexicon = parseTypeLexicon types

    -- Ignore this for now. This will be used
    --  when we implement subtyping.
    -- _ <- many subtypeDeclaration

    case typeLexicon of
        SomeTypeLexicon _ parse -> do
            atoms <- many (documentedEntity parse)
            productions <- many productionDeclaration

            case parseSomeLexicon parse atoms productions of
                Left err -> fail $ show err
                Right lexicon -> return lexicon