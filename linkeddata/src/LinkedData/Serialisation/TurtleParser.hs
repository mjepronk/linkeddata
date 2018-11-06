-- | Turtle parser
--
-- See:
-- - https://www.w3.org/TR/turtle/#sec-grammar-grammar
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module LinkedData.Serialisation.TurtleParser
  ( parseTurtle
  , parseTurtleFile
  )
where

import           Prelude hiding (takeWhile)

import           Data.Char (isDigit, isHexDigit)
import           Data.Foldable (traverse_)
import           Data.Functor (($>))
import           Data.Monoid ((<>))
import           Control.Applicative ((<|>))
import           Control.Monad (void)
import           Control.Monad.Trans.Resource (MonadResource)
import           Control.Monad.Trans.State.Strict (StateT, gets, modify')
import           Data.Attoparsec.Combinator (lookAhead)
import           Data.Attoparsec.Text (Parser, (<?>), asciiCI, char, manyTill,
                     option, satisfy, sepBy, string, takeWhile, takeWhile1, try)
import qualified Data.ByteString.Streaming as B
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import           Streaming (Stream, Of, lift)
import qualified Streaming.Prelude as S
import           Text.Parser.Char (oneOf)
import           Text.Parser.Combinators (many, notFollowedBy, optional, some,
                     sepEndBy1, eof)

import           LinkedData.Types
import           LinkedData.IRI (IRI, Abs, Prefix(..), mkAbsIRI, mkRelIRI,
                     mkIRIWithBase, (.:.))
import           LinkedData.Serialisation.Common (pStringLiteralQuote,
                     pUChar, pEChar, pLangTag, pBlankNodeLabel, isPNCharsBase,
                     isPNCharsU, isPNChars)
import           LinkedData.Serialisation.Streaming (parsedWith,
                     decodeByteString, decodeUtf8Pure, handleDecodeError)
import           LinkedData.Namespaces (rdfType, rdfFirst, rdfRest, rdfNil,
                     xsdBoolean, xsdInteger, xsdDecimal, xsdDouble)


type TurtleParser = StateT ParserState Parser


-- | Parsing state for the Turtle parser.
--
-- See:
-- - https://www.w3.org/TR/turtle/#sec-grammar-grammar#h3_sec-parsing-state

data ParserState = ParserState
    { stBase         :: Maybe (IRI Abs)
    , stNamespaces   :: Namespaces
    , stBlankNodeID  :: Int
    , stCurSubject   :: Maybe Term
    , stCurPredicate :: Maybe Term
    , stTriples      :: Triples
    }
  deriving Show

initialParserState :: ParserState
initialParserState = ParserState
    { stBase         = Nothing
    , stNamespaces   = M.empty
    , stBlankNodeID  = 0
    , stCurSubject   = Nothing
    , stCurPredicate = Nothing
    , stTriples      = []
    }


-- | Consume strict 'T.Text' values in Turtle format and yield parsed triples.
-- When a parsing or decoding error occurs a Left SerialisationError is
-- returned.
parseTurtle
  :: Monad m
  => Maybe (IRI Abs)  -- ^ Optional base IRI
  -> Stream (Of T.Text) m r
  -> Stream (Of Triple) m (Either SerialisationError GraphMeta)
parseTurtle base s = parseTurtle' state (s $> Right ())
  where state = initialParserState { stBase = base }


-- | Read a Turtle file and yield parsed triples. When a parsing or decoding
-- error occurs a Left SerialisationError is returned.
parseTurtleFile
  :: MonadResource m
  => Maybe (IRI Abs)  -- ^ Optional base IRI
  -> FilePath  -- ^ Path to file
  -> Stream (Of Triple) m (Either SerialisationError GraphMeta)
parseTurtleFile base fp = parseTurtle' state (readFile' fp)
  where
    state = initialParserState { stBase = base <|> mkAbsIRI (T.pack ("file://" <> fp)) }
    readFile' fp' = decodeByteString decodeUtf8Pure (B.readFile fp') >>= handleDecodeError


-- | Internal helper for parseTurtle and parseTurtleFile.
parseTurtle'
  :: Monad m
  => ParserState
  -> Stream (Of T.Text) m (Either SerialisationError ())
  -> Stream (Of Triple) m (Either SerialisationError GraphMeta)
parseTurtle' state p = do
    result <- S.concat (parsedWith parser state p)
    case result of
      Right (_, s) -> pure $ Right GraphMeta {
           base=stBase s
         , namespaces=stNamespaces s }
      Left (err, unparsed) -> do
        t <- lift $ S.next unparsed
        case t of
          Right (t', _) -> pure . Left $ SParsingError t' [] (show err)
          Left e -> pure $ const emptyGraphMeta <$> e
  where
    parser = lift pWS *> pStatement <* lift pWS


-- Functions that work on the parser state.
addNamespace :: Prefix -> IRI Abs -> TurtleParser ()
addNamespace p i =
  modify' (\st@ParserState { stNamespaces } -> st {
    stNamespaces=M.insert p i stNamespaces })

lookupNamespace :: Prefix -> TurtleParser (Maybe (IRI Abs))
lookupNamespace p =
  gets (\ParserState { stNamespaces } -> M.lookup p stNamespaces)

setBase :: IRI Abs -> TurtleParser ()
setBase iri = modify' (\st -> st { stBase=Just iri })

getCurSubject :: TurtleParser Term
getCurSubject = do
  s <- gets stCurSubject
  case s of
    Just s' -> pure s'
    Nothing -> fail "No subject encountered yet!"

setCurSubject :: Maybe Term -> TurtleParser ()
setCurSubject s =
  modify' (\st -> st { stCurSubject=s })

getCurPredicate :: TurtleParser Term
getCurPredicate = do
  p <- gets stCurPredicate
  case p of
    Just p' -> pure p'
    Nothing -> fail "No predicate encountered yet!"

setCurPredicate :: Maybe Term -> TurtleParser ()
setCurPredicate p =
  modify' (\st -> st { stCurPredicate=p })

withSubjPred :: Maybe Term -> Maybe Term -> TurtleParser a -> TurtleParser a
withSubjPred s p parser = do
    (s', p') <- gets(\ParserState { stCurSubject, stCurPredicate } -> (stCurSubject, stCurPredicate))
    modify' (\st -> st { stCurSubject=s, stCurPredicate=p })
    r <- parser
    modify' (\st -> st { stCurSubject=s', stCurPredicate=p' })
    pure r

getNextBlankNodeID :: TurtleParser Int
getNextBlankNodeID = do
  i <- gets (\ParserState { stBlankNodeID } -> stBlankNodeID + 1)
  modify' (\st -> st{ stBlankNodeID=i })
  pure i

getTriples :: TurtleParser Triples
getTriples = do
    ts <- gets stTriples
    modify' (\st -> st { stTriples=[] })
    pure (reverse ts)

addTriple :: Triple -> TurtleParser ()
addTriple t = do
    modify' (\st@ParserState { stTriples } -> st { stTriples=t : stTriples })
    pure ()


-- | [1] turtleDoc
pTurtleDoc :: TurtleParser Graph
pTurtleDoc = do
  ts <- mconcat <$> manyTill (lift pWS *> pStatement <* lift pWS) eof
  ns <- gets stNamespaces
  base <- gets stBase
  pure Graph { triples = ts, meta = GraphMeta { namespaces = ns, base = base } }

-- | [2] statement
pStatement :: TurtleParser Triples
pStatement = pDirective *> pure [] <|>
             pTriples <* lift (pWS <* char '.'<?> "RDF statement")

-- | [3] directive
pDirective :: TurtleParser ()
pDirective = pPrefixID <|> pBase <|> pSparqlPrefix <|> pSparqlBase

-- | [4] prefixID
pPrefixID :: TurtleParser ()
pPrefixID = do
  prefix <- lift (string "@prefix" *> pWS1 *> pPNameNS <* pWS1)
  iri <- pIRIRef <* lift (pWS <* char '.')
  addNamespace prefix iri

-- | [5] base
pBase :: TurtleParser ()
pBase = do
  iri <- lift (string "@base" *> pWS1) *> pIRIRef  <* lift (pWS <* char '.')
  setBase iri

-- | [5s] sparqlBase
pSparqlBase :: TurtleParser ()
pSparqlBase = do
  iri <- lift (asciiCI "BASE" *> pWS1) *> pIRIRef <* lift pWS1
  setBase iri

-- | [6s] sparqlPrefix
pSparqlPrefix :: TurtleParser ()
pSparqlPrefix = do
  prefix <- lift (asciiCI "PREFIX" *> pWS1 *> pPNameNS <* pWS1)
  iri <- pIRIRef
  addNamespace prefix iri

-- | [6] triples
pTriples :: TurtleParser Triples
pTriples = do
  ts <- (pSubject *> lift pWS *> pPredicateObjectList)
        <|> (do
          b <- pBlankNodePropertyList
          _ <- lift pWS
          option [] (withSubjPred (Just b) Nothing pPredicateObjectList))
  ts' <- getTriples
  setCurSubject Nothing
  setCurPredicate Nothing
  pure $ ts <> ts'

-- | [7] predicateObjectList
pPredicateObjectList :: TurtleParser Triples
pPredicateObjectList = mconcat <$> predicateObjectList `sepEndBy1` many (lift (char ';' <* pWS))
  where
    predicateObjectList :: TurtleParser Triples
    predicateObjectList = pVerb *> lift pWS1 *> pObjectList <* lift pWS

-- | [8] objectList
pObjectList :: TurtleParser Triples
pObjectList = (pObject <* lift pWS) `sepBy` lift (char ',' <* pWS)

-- | [9] verb (this parser sets stCurPredicate)
pVerb :: TurtleParser Term
pVerb = do
    p <- pPredicate <|> pA -- <?> "verb"
    setCurPredicate (Just p)
    pure p
  where
    pA :: TurtleParser Term
    pA = lift (char 'a') *> pure (ITerm rdfType)

-- | [10] subject (this parser sets stCurSubject)
pSubject :: TurtleParser Term
pSubject = do
    s <- (ITerm <$> pIRI) <|> pBlankNode <|> pCollection
    setCurSubject (Just s)
    pure s

-- | [11] predicate
pPredicate :: TurtleParser Term
pPredicate = ITerm <$> pIRI

-- | [12] object
-- Each object N in the document produces an RDF triple:
--   curSubject curPredicate N .
pObject :: TurtleParser Triple
pObject = do
    s <- getCurSubject
    p <- getCurPredicate
    o <- (ITerm <$> pIRI)
         <|> pBlankNodePropertyList
         <|> pBlankNode
         <|> pCollection
         <|> pLiteral
    pure (Triple s p o)

-- | [13] literal
pLiteral :: TurtleParser Term
pLiteral = pRDFLiteral <|> pNumericLiteral <|> pBooleanLiteral -- <?> "literal"

-- | [14] blankNodePropertyList
-- Beginning the `blankNodePropertyList` production records the `curSubject` and
-- `curPredicate`, and sets `curSubject` to a novel blank node `B`. Finishing
-- the `blankNodePropertyList` production restores `curSubject` and
-- `curPredicate`. The node produced by matching `blankNodePropertyList` is the
-- blank node `B`.
pBlankNodePropertyList :: TurtleParser Term
pBlankNodePropertyList = do
    _ <- lift (char '[' <* pWS <?> "'[' start of blank node property list")
    b <- BNodeGen <$> getNextBlankNodeID
    ts <- withSubjPred (Just b) Nothing pPredicateObjectList
    traverse_ addTriple ts
    _ <- lift (char ']' <* pWS)
    pure b

-- | [15] collection
-- Beginning the collection production records the `curSubject` and
-- `curPredicate`. Each object in the collection production has a `curSubject`
-- set to a novel blank node `B` and a curPredicate set to `rdf:first`. For each
-- object object `n` after the first produces a triple:
--
--   object`n-1` rdf:rest object`n` .
--
-- Finishing the collection production creates an additional triple:
--
--   curSubject rdf:rest rdf:nil .
--
-- and restores `curSubject` and `curPredicate`. The node produced by matching
-- collection is the first blank node `B` for non-empty lists and `rdf:nil` for
-- empty lists.
pCollection :: TurtleParser Term
pCollection = do
    _ <- lift (char '(')
    firstNode <- do
      b <- BNodeGen <$> getNextBlankNodeID
      nextTriple <- withSubjPred (Just b) (Just . ITerm $ rdfFirst) (lift pWS *> optional pObject)
      case nextTriple of
        Just nextTriple' -> do
          addTriple nextTriple'
          go b
          pure b
        Nothing ->
          -- Empty collection
          pure $ ITerm rdfNil
    _ <- lift (pWS *> char ')' <* pWS)
    pure firstNode
  where
    go :: Term -> TurtleParser ()
    go prevNode = do
      b <- BNodeGen <$> getNextBlankNodeID
      nextTriple <- withSubjPred (Just b) (Just . ITerm $ rdfFirst) (lift pWS *> optional pObject)
      case nextTriple of
        Just nextTriple' -> do
          addTriple $ Triple prevNode (ITerm rdfRest) b
          addTriple nextTriple'
          go b
        Nothing ->
          addTriple $ Triple prevNode (ITerm rdfRest) (ITerm rdfNil)

-- | [16] NumericLiteral
pNumericLiteral :: TurtleParser Term
pNumericLiteral = lift pInteger <|> lift  pDecimal <|> lift pDouble

-- | [128s] RDFLiteral
pRDFLiteral :: TurtleParser Term
pRDFLiteral = do
  s <- pString
  l <- optional . lift $ pLangTag
  case l of
    Just l' -> pure (plainLL s l')
    Nothing -> do
      t <- optional (lift (string "^^") *> pIRI)
      case t of
        Just t' -> pure (typedL s t')
        Nothing -> pure (plainL s)

-- | [133s] BooleanLiteral
pBooleanLiteral :: TurtleParser Term
pBooleanLiteral = do
  l <- lift (string "true" <|> string "false")
  pure $ typedL l (xsdBoolean)

-- | [17] String
pString :: TurtleParser T.Text
pString = lift pStringLiteralLongSingleQuote
      <|> lift pStringLiteralLongQuote
      <|> lift pStringLiteralQuote
      <|> lift pStringLiteralSingleQuote

-- | [135s] iri
pIRI :: TurtleParser (IRI Abs)
pIRI = pIRIRef <|> pPrefixedName

-- | [136s] PrefixedName
pPrefixedName :: TurtleParser (IRI Abs)
pPrefixedName = pPNameLN <|> pPNameNS'
  where
    pPNameNS' = do
      p <- lift pPNameNS
      iri <- lookupNamespace p
      case iri of
        Just i -> pure i
        Nothing -> fail $ "Namespace '" <> show p <> "' is not defined."

-- | [137s] BlankNode
pBlankNode :: TurtleParser Term
pBlankNode = lift pBlankNodeLabel <|> pAnon

-- | [18] IRIREF
pIRIRef :: TurtleParser (IRI Abs)
pIRIRef = do
    iri <- lift (char '<' *> pIRI' <* char '>')
    base <- gets stBase
    case mkIRIWithBase base iri of
      Left err -> fail err
      Right iri' -> pure iri'
  where
    pIRI' = mconcat <$> many (pIRIChar <|> pUChar)
    pIRIChar = takeWhile1 (\c -> c > '\x0020' && c `notElem` ['<', '>', '"', '{', '}', '|', '^', '`', '\\'])

-- | [139s] PNAME_NS
pPNameNS :: Parser Prefix
pPNameNS = do
   p <- option "" pPNPrefix
   _ <- char ':'
   pure (Prefix p)

-- | [140s] PNAME_LN
pPNameLN :: TurtleParser (IRI Abs)
pPNameLN = do
    prefix <- lift pPNameNS
    local <- lift pPNLocal
    iri <- lookupNamespace prefix
    case iri of
      Nothing -> fail $ "Namespace '" <> show prefix <> "' is not defined."
      Just iri' ->
        case mkRelIRI local of
          Just local' -> pure $ iri' .:. local'
          Nothing -> fail $ "Invalid relative IRI <" <> T.unpack local <> ">."

-- Numeric helpers
pSign :: Parser T.Text
pSign = option "" (T.singleton <$> oneOf "+-")

pDigits :: Parser T.Text
pDigits = takeWhile isDigit

pDigits1 :: Parser T.Text
pDigits1 = takeWhile1 isDigit

pDot :: Parser T.Text
pDot = T.singleton <$> char '.'

-- | [19] INTEGER
pInteger :: Parser Term
pInteger = do
    s <- pSign
    x <- pDigits1
    notFollowedBy (pDot <|> pExponent)
    pure $ typedL (s <> x) xsdInteger

-- | [20] DECIMAL
pDecimal :: Parser Term
pDecimal = do
    x <- mconcat <$> sequence [pSign, pDigits, pDot, pDigits1]
    notFollowedBy pExponent
    pure $ typedL x xsdDecimal

-- | [21] DOUBLE
pDouble :: Parser Term
pDouble = do
    s <- pSign
    x <- try (mconcat <$> sequence [pDigits1, pDot, pDigits, pExponent])
         <|> try (mconcat <$> sequence [pDot, pDigits1, pExponent])
         <|> mconcat <$> sequence [pDigits1, pExponent]
    pure $ typedL (s <> x) xsdDouble

-- | [154s] EXPONENT
pExponent :: Parser T.Text
pExponent = mconcat <$> sequence [ T.singleton <$> oneOf "eE", pSign, pDigits1]

-- | [23] STRING_LITERAL_SINGLE_QUOTE
pStringLiteralSingleQuote :: Parser T.Text
pStringLiteralSingleQuote = char '\'' *> p <* char '\''
  where p = mconcat <$> many (takeWhile1 (`notElem` ['\'', '\\', '\n', '\r']) <|> try pEChar <|> pUChar)

-- | [24] STRING_LITERAL_LONG_SINGLE_QUOTE
pStringLiteralLongSingleQuote :: Parser T.Text
pStringLiteralLongSingleQuote = string "'''" *> (mconcat <$> many p) <* string "'''"
  where p = (<>) <$> (try (string "'" <* notFollowedBy (char '\''))
                           <|> try (string "''"  <* notFollowedBy (char '\''))
                           <|> pure "")
                 <*> (takeWhile1 (`notElem` ['\'', '\\'])) <|> try pEChar <|> pUChar

-- | [25] STRING_LITERAL_LONG_QUOTE
pStringLiteralLongQuote :: Parser T.Text
pStringLiteralLongQuote = string "\"\"\"" *> (mconcat <$> many p) <* string "\"\"\""
  where p = (<>) <$> (try (string "\"" <* notFollowedBy (char '"'))
                           <|> try (string "\"\""  <* notFollowedBy (char '"'))
                           <|> pure "")
                 <*> (takeWhile1 (`notElem` ['"', '\\'])) <|> try pEChar <|> pUChar

-- | [161s] WS
-- Optional whitepace or comment.
pWS :: Parser ()
pWS = void $ many (pWS1' <|> pComment)

-- | Mandatory whitespace or comment.
pWS1 :: Parser ()
pWS1 = void $ some (pWS1' <|> pComment)

-- | Whitespace subparser for pWS and pWS1.
pWS1' :: Parser ()
pWS1' = void $ takeWhile1 (`elem` [' ', '\t', '\r', '\n'])

-- | Parse a comment.
pComment :: Parser ()
pComment = void $ char '#' *> takeWhile (`notElem` ['\r', '\n'])

-- [162s] ANON
pAnon :: TurtleParser Term
pAnon = do
  _ <- lift (char '[' <* pWS <* char ']' <?> "anonymous node")
  BNodeGen <$> getNextBlankNodeID

-- [167s]
pPNPrefix :: Parser T.Text
pPNPrefix = do
    x <- T.singleton <$> satisfy isPNCharsBase
    xs <- mconcat <$> many (
      pn_chars <|>
      (T.singleton <$> char '.' <* lookAhead pn_chars))
    pure (x <> xs)
  where
    pn_chars = takeWhile1 isPNChars

-- [168s] PN_LOCAL
pPNLocal :: Parser T.Text
pPNLocal = do
    x <- T.singleton <$> satisfy (\c ->
      isPNCharsU c || c == ':' || isDigit c) <|> pPLX
    xs <- mconcat <$> many (
      pn_chars_colon_plx <|>
      (T.singleton <$> char '.' <* lookAhead pn_chars_colon_plx))
    pure (x <> xs)
  where
    pn_chars_colon_plx = takeWhile1 (\c -> isPNChars c || c == ':') <|> pPLX

-- | [169s] PLX
pPLX :: Parser T.Text
pPLX = pPercent <|> pPNLocalEsc

-- | [170s] PERCENT (Note: this should not be decoded by the parser!)
pPercent :: Parser T.Text
pPercent = T.pack <$> sequence [char '%', satisfy isHexDigit, satisfy isHexDigit]

-- | [171s] PN_LOCAL_ESC
pPNLocalEsc :: Parser T.Text
pPNLocalEsc = char '\\' *> (T.singleton <$> oneOf escChars)
  where escChars = [
          '_', '~', '.', '-', '!', '$', '&', '\'', '(', ')',
          '*', '+', ',', ';', '=', '/', '?', '#', '@', '%']
