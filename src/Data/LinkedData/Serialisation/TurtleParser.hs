-- | Turtle parser
--
-- See:
-- - https://www.w3.org/TR/turtle/#sec-grammar-grammar
--

-- TODO:
-- - addTriple / getTriple: use datastructure that supports cheap appends

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.LinkedData.Serialisation.TurtleParser where

import           Data.Maybe (Maybe(..))
import           Data.Char (digitToInt, chr, isDigit, isAsciiLower, isAsciiUpper)
import           Data.Foldable (find, foldl', concat, traverse_)
import           Data.Monoid ((<>))
import           Control.Monad (void)
import           Control.Monad.State.Strict (State, runState, gets, modify')
import           Control.Applicative ((<|>))
import           Control.Applicative.Combinators (sepBy, sepEndBy1, between,
                     count, option, optional, many, some, manyTill)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Map.Strict as M
import           Text.Megaparsec ((<?>), ParsecT, ParseError(..), runParserT, try,
                     customFailure, takeWhileP, takeWhile1P, eof, notFollowedBy, label)
import           Text.Megaparsec.Char (char, hexDigitChar, oneOf, string, string')
import           Data.LinkedData.Types
import           Data.LinkedData.IRI as LD


type TurtleParser = ParsecT T.Text TL.Text (State ParserState)

-- | Parsing state for the Turtle parser.
-- See:
-- - https://www.w3.org/TR/turtle/#sec-grammar-grammar#h3_sec-parsing-state
data ParserState = ParserState
       { stBase         :: Maybe IRI
       , stNamespaces   :: M.Map Prefix IRI
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

parseTurtle :: FilePath -> TL.Text -> Either (ParseError Char T.Text) Graph
parseTurtle fp t =
    case runState (runParserT pTurtleDoc fp t) s of
      (Left e, _)  -> Left e
      (Right x, _) -> Right x
  where s = initialParserState { stBase = Just . IRI . T.pack $ fp }

parseTurtleFile :: FilePath -> IO (Either (ParseError Char T.Text) Graph)
parseTurtleFile fp = parseTurtle fp . TL.decodeUtf8 <$> BL.readFile fp


-- Functions that work on the parser state.
addNamespace :: Prefix -> IRI -> TurtleParser ()
addNamespace p i =
  modify' (\st@ParserState { stNamespaces } -> st {
    stNamespaces=M.insert p i stNamespaces })

lookupNamespaceIRI :: Prefix -> TurtleParser (Maybe IRI)
lookupNamespaceIRI p =
  gets (\ParserState { stNamespaces } -> M.lookup p stNamespaces)

setBaseURI :: IRI -> TurtleParser ()
setBaseURI i =
  modify' (\st -> st { stBase=Just i })

getCurSubject :: TurtleParser Term
getCurSubject = do
  s <- gets stCurSubject
  case s of
    Just s' -> pure s'
    Nothing -> customFailure "No subject encountered yet!"

setCurSubject :: Maybe Term -> TurtleParser ()
setCurSubject s =
  modify' (\st -> st { stCurSubject=s })

getCurPredicate :: TurtleParser Term
getCurPredicate = do
  p <- gets stCurPredicate
  case p of
    Just p' -> pure p'
    Nothing -> customFailure "No predicate encountered yet!"

setCurPredicate :: Maybe Term -> TurtleParser ()
setCurPredicate p =
  modify' (\st -> st { stCurPredicate=p })

getNextBlankNodeID :: TurtleParser Int
getNextBlankNodeID = do
  i <- gets (\ParserState { stBlankNodeID } -> stBlankNodeID + 1)
  modify' (\st -> st{ stBlankNodeID=i })
  pure i

withSubjPred :: Maybe Term
             -> Maybe Term
             -> TurtleParser a
             -> TurtleParser a
withSubjPred s p parser = do
    (s', p') <- gets(\ParserState { stCurSubject, stCurPredicate } -> (stCurSubject, stCurPredicate))
    modify' (\st -> st { stCurSubject=s, stCurPredicate=p })
    r <- parser
    modify' (\st -> st { stCurSubject=s', stCurPredicate=p' })
    pure r

getTriples :: TurtleParser Triples
getTriples = do
    ts <- gets stTriples
    modify' (\st -> st { stTriples=[] })
    pure $ reverse ts

addTriple :: Triple -> TurtleParser ()
addTriple t = do
    modify' (\st@ParserState { stTriples } -> st { stTriples=t : stTriples  })
    pure ()


-- | [1] turtleDoc
pTurtleDoc :: TurtleParser Graph
pTurtleDoc = do
  ts <- mconcat <$> manyTill (pWS *> pStatement <* pWS) eof
  ns <- gets stNamespaces
  bs <- gets stBase
  pure Graph { triples = ts, namespaces = ns, base = bs }

-- | [2] statement
pStatement :: TurtleParser Triples
pStatement = pDirective *> pure [] <|>
             pTriples <* pWS <* char '.' <?> "RDF statement"

-- | [3] directive
pDirective :: TurtleParser ()
pDirective = pPrefixID <|> pBase <|> pSparqlPrefix <|> pSparqlBase

-- | [4] prefixID
pPrefixID :: TurtleParser ()
pPrefixID = do
  prefix <- string "@prefix" *> pWS1 *> pPNameNS <* pWS1
  iri <- pIRIRef <* pWS <* char '.'
  addNamespace prefix iri

-- | [5] base
pBase :: TurtleParser ()
pBase = do
  iri <- string "@base" *> pWS1 *> pIRIRef <* pWS <* char '.'
  setBaseURI iri

-- | [5s] sparqlBase
pSparqlBase :: TurtleParser ()
pSparqlBase = do
  iri <- string' "BASE" *> pWS1 *> pIRIRef <* pWS1
  setBaseURI iri

-- | [6s] sparqlPrefix
pSparqlPrefix :: TurtleParser ()
pSparqlPrefix = do
  prefix <- string' "PREFIX" *> pWS1 *> pPNameNS <* pWS1
  iri <- pIRIRef
  addNamespace prefix iri

-- | [6] triples
pTriples :: TurtleParser Triples
pTriples = do
  ts <- (try (pSubject <* pWS *> pPredicateObjectList))
        <|> (pBlankNodePropertyList <* pWS *> option [] pPredicateObjectList)
  ts' <- getTriples
  setCurSubject Nothing
  setCurPredicate Nothing
  pure $ ts <> ts'

-- | [7] predicateObjectList
pPredicateObjectList :: TurtleParser Triples
pPredicateObjectList = concat <$> predicateObjectList `sepEndBy1` (many (char ';' <* pWS))
  where
    predicateObjectList :: TurtleParser Triples
    predicateObjectList = pVerb *> pWS1 *> pObjectList <* pWS

-- | [8] objectList
pObjectList :: TurtleParser Triples
pObjectList = (pObject <* pWS) `sepBy` (char ',' <* pWS)

-- | [9] verb (this parser sets stCurPredicate)
pVerb :: TurtleParser Term
pVerb = do
    p <- pPredicate <|> pA <?> "verb"
    setCurPredicate (Just p)
    pure p
  where
    pA :: TurtleParser Term
    pA = char 'a' *> pure (iterm rdfNS (IRI "type"))

-- | [10] subject (this parser sets stCurSubject)
pSubject :: TurtleParser Term
pSubject = do
    s <- (iriToTerm <$> pIRI) <|> pBlankNode <|> pCollection
    setCurSubject (Just s)
    pure s

-- | [11] predicate
pPredicate :: TurtleParser Term
pPredicate = iriToTerm <$> pIRI

-- | [12] object
-- Each object N in the document produces an RDF triple: curSubject curPredicate N.
pObject :: TurtleParser Triple
pObject = do
    o <- (iriToTerm <$> pIRI)
         <|> try pBlankNodePropertyList
         <|> pBlankNode
         <|> pCollection
         <|> pLiteral
    s <- getCurSubject
    p <- getCurPredicate
    case triple s p o of
      Right t -> pure t
      Left e -> customFailure $ (T.pack (show (s, p, o))) <> ": " <> e

-- | [13] literal
pLiteral :: TurtleParser Term
pLiteral = pRDFLiteral <|> pNumericLiteral <|> pBooleanLiteral <?> "literal"

-- | [14] blankNodePropertyList
-- Beginning the `blankNodePropertyList` production records the `curSubject` and
-- `curPredicate`, and sets `curSubject` to a novel blank node `B`. Finishing
-- the `blankNodePropertyList` production restores `curSubject` and
-- `curPredicate`. The node produced by matching `blankNodePropertyList` is the
-- blank node `B`.
pBlankNodePropertyList :: TurtleParser Term
pBlankNodePropertyList = do
  void (char '[' <?> "'[' start of blank node property list")
  blank <- BNodeGen <$> getNextBlankNodeID
  ts <- withSubjPred (Just blank) Nothing pPredicateObjectList
  traverse_ addTriple ts
  void $ char ']'
  pure blank

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
    void $ char '('
    blank <- BNodeGen <$> getNextBlankNodeID
    _ <- go blank
    void $ pWS *> char ')' <* pWS
    pure blank -- TODO should return rdfNil for empty list
  where
    go :: Term -> TurtleParser Term
    go n = do
      t <- withSubjPred (Just n) (Just rdfFirst) (pWS *> optional pObject)
      case t of
        Just t' -> do
          addTriple t'
          n' <- BNodeGen <$> getNextBlankNodeID
          addTriple $ Triple n rdfRest n'
          go n'
        Nothing -> do
          addTriple $ Triple n rdfRest rdfNil
          pure n

    rdfFirst = iterm rdfNS (IRI "first")
    rdfRest = iterm rdfNS (IRI "rest")
    rdfNil = iterm rdfNS (IRI "nil")

-- | [16] NumericLiteral
pNumericLiteral :: TurtleParser Term
pNumericLiteral = try pInteger <|> try pDecimal <|> pDouble

-- | [128s] RDFLiteral
pRDFLiteral :: TurtleParser Term
pRDFLiteral = do
  s <- pString
  l <- optional pLangTag
  case l of
    Just l' -> pure (plainLL s l')
    Nothing -> do
      t <- optional (string "^^" *> pIRI)
      case t of
        Just t' -> pure (typedL s t')
        Nothing -> pure (plainL s)

-- | [133s] BooleanLiteral
pBooleanLiteral :: TurtleParser Term
pBooleanLiteral = do
  l <- TL.toStrict <$> (string "true" <|> string "false")
  pure $ typedL l (xsdNS `unsafeAppendIRI` (IRI "boolean"))

-- | [17] String
pString :: TurtleParser T.Text
pString = pStringLiteralLongSingleQuote
      <|> pStringLiteralLongQuote
      <|> pStringLiteralQuote
      <|> pStringLiteralSingleQuote

-- | [135s] iri
pIRI :: TurtleParser IRI
pIRI = pIRIRef <|> pPrefixedName

-- | [136s] PrefixedName
pPrefixedName :: TurtleParser IRI
pPrefixedName =
    label "PrefixedName: PNAME_LN'" pPNameLN <|>
    label "PrefixedName: PNAME_NS'" pPNameNS'
  where
    pPNameNS' = do
      p <- pPNameNS
      iri <- lookupNamespaceIRI p
      case iri of
        Just i -> pure i
        Nothing -> customFailure $ "namespace '" <> T.pack (show p) <> "' is not defined."

-- | [137s] BlankNode
pBlankNode :: TurtleParser Term
pBlankNode = pBlankNodeLabel <|> pAnon

-- | [18] IRIREF
pIRIRef :: TurtleParser IRI
pIRIRef = do
    iri <- IRI <$> between (char '<') (char '>') iriChars
    case isAbsoluteIRI iri of
      Just True -> pure iri
      Just False -> do
        base <- gets stBase
        case base of
          Just base' ->
            case LD.appendIRI base' iri of
              Just iri' -> pure iri'
              Nothing -> customFailure $ "Could not resolve relative IRI '" <> unIRI iri <> "'."
          Nothing -> customFailure $ "Relative IRI '" <> unIRI iri <> "' found, but there is no base IRI defined."
      Nothing -> customFailure $ "Could not determine if IRI '" <> unIRI iri <> "' is absolute."
  where
    iriChars = TL.toStrict <$> takeWhileP
      (Just "IRI char")
      ( \c -> c > '\x0020' && c `notElem` ['<', '>', '"', '{', '}', '|', '^', '`', '\\'])

-- | [139s] PNAME_NS
pPNameNS :: TurtleParser Prefix
pPNameNS = Prefix <$> option "" pPNPrefix <* char ':'

-- | [140s] PNAME_LN
pPNameLN :: TurtleParser IRI
pPNameLN = do
  prefix <- pPNameNS
  prefixIRI <- lookupNamespaceIRI prefix
  case prefixIRI of
    Just i -> do
      local <- IRI <$> pPNLocal
      case LD.appendIRI i local of
        Just iri -> pure iri
        Nothing -> customFailure $ "could not join IRI's: '" <> unIRI i <> "' and '" <> T.pack (show local) <> "'."
    Nothing -> customFailure $ "namespace '" <> T.pack (show prefix) <> "' is not defined."

-- | [141s] BLANK_NODE_LABEL
-- TODO remove partial head and last
pBlankNodeLabel :: TurtleParser Term
pBlankNodeLabel = do
  _ <- string "_:"
  l <- TL.toStrict <$> takeWhile1P Nothing isPNChars
  let hd = T.head l
      lt = T.last l
  if isPNCharsU hd || isDigit hd
    then if isPNChars lt
      then pure (BNode l)
      else customFailure $ "blank node label may not end with '" <> (T.singleton lt) <> "'."
    else customFailure $ "blank node label may not start with '" <> (T.singleton hd) <> "'."

-- | [144s] LANGTAG
-- TODO remove partial head and last
pLangTag :: TurtleParser T.Text
pLangTag = do
    _ <- char '@'
    s <- TL.toStrict <$> takeWhileP (Just "language identifier") validChar
    if validFirstChar (T.head s)
      then if validLastChar (T.last s)
        then pure s
        else customFailure "language identifier may not end with a dash."
      else customFailure "language identifier must start with a letter."
  where
    validChar c = c == '-' || isAsciiLower c || isAsciiUpper c || isDigit c
    validFirstChar c = isAsciiLower c || isAsciiUpper c
    validLastChar c = c /= '-'

-- | [19] INTEGER
pInteger :: TurtleParser Term
pInteger = do
  s <- option "" (T.singleton <$> (oneOf ['+', '-']))
  i <- TL.toStrict <$> takeWhile1P Nothing isDigit
  notFollowedBy (char '.')
  pure $ typedL (s <> i) (xsdNS `unsafeAppendIRI` (IRI "integer"))

-- | [20] DECIMAL
pDecimal :: TurtleParser Term
pDecimal = do
  s <- option "" (T.singleton <$> (oneOf ['+', '-']))
  i <- TL.toStrict <$> takeWhileP Nothing isDigit
  _ <- char '.'
  d <- TL.toStrict <$> takeWhile1P Nothing isDigit
  notFollowedBy pExponent
  pure $ typedL (s <> i <> "." <> d) (xsdNS `unsafeAppendIRI` (IRI "decimal"))

-- [21] DOUBLE
pDouble :: TurtleParser Term
pDouble = do
    s <- option "" (T.singleton <$> (oneOf ['+', '-']))
    i <- TL.toStrict <$> takeWhileP Nothing isDigit
    _ <- char '.'
    d <- TL.toStrict <$> takeWhileP Nothing isDigit
    e <- pExponent
    if T.null i && T.null d
      then customFailure $ "invalid double format"
      else pure $ typedL (s <> i <> "." <> d <> e) (xsdNS `unsafeAppendIRI` (IRI "double"))

-- [154s] TODO
pExponent :: TurtleParser T.Text
pExponent = do
  e <- T.singleton <$> oneOf ['e', 'E']
  s <- option "" (T.singleton <$> (oneOf ['+', '-']))
  x <- TL.toStrict <$> takeWhile1P Nothing isDigit
  pure $ e <> s <> x

-- [22] STRING_LITERAL_QUOTE
pStringLiteralQuote :: TurtleParser T.Text
pStringLiteralQuote = between (char '"') (char '"') p
  where p = mconcat <$> many (TL.toStrict <$> takeWhile1P Nothing (`notElem` ['"', '\\', '\n', '\r']) <|> pEChar <|> pUChar)

-- [23] STRING_LITERAL_SINGLE_QUOTE
pStringLiteralSingleQuote :: TurtleParser T.Text
pStringLiteralSingleQuote = between (char '\'') (char '\'') p
  where p = mconcat <$> many (TL.toStrict <$> takeWhile1P Nothing (`notElem` ['\'', '\\', '\n', '\r']) <|> pEChar <|> pUChar)

-- [24] STRING_LITERAL_LONG_SINGLE_QUOTE
pStringLiteralLongSingleQuote :: TurtleParser T.Text
pStringLiteralLongSingleQuote = between (string "'''") (string "'''") (mconcat <$> many p)
  where p = (<>) <$> TL.toStrict <$> (string "''" <|> string "'" <|> pure "")
                 <*> (TL.toStrict <$> (takeWhile1P Nothing (`notElem` ['\'', '\\']))) <|> pEChar <|> pUChar

-- [25] STRING_LITERAL_LONG_QUOTE
pStringLiteralLongQuote :: TurtleParser T.Text
pStringLiteralLongQuote = between (string "\"\"\"") (string "\"\"\"") (mconcat <$> many p)
  where p = (<>) <$> TL.toStrict <$> (string "\"\"" <|> string "\"" <|> pure "")
                 <*> (TL.toStrict <$> (takeWhile1P Nothing (`notElem` ['"', '\\']))) <|> pEChar <|> pUChar

-- [26] UCHAR
pUChar :: TurtleParser T.Text
pUChar = do
    uchar <- try uchar16 <|> uchar32
    pure $ T.singleton (chr (hexToInt uchar))
  where
    uchar16 :: TurtleParser String
    uchar16 = char '\\' *> char 'u' *> count 4 hexDigitChar

    uchar32 :: TurtleParser String
    uchar32 = char '\\' *> char 'U' *> count 8 hexDigitChar

    hexToInt :: String -> Int
    hexToInt = foldl' ((+) . (16 *)) 0 . map digitToInt

-- | [159s] ECHAR
pEChar :: TurtleParser T.Text
pEChar = do
    _ <- char '\\'
    c <- oneOf ['t', 'b', 'n', 'r', 'f', '"', '\'', '\\']
    case find (\(f, _) -> f == c) codeToCharMap of
      Just (_, t') -> pure (T.singleton t')
      Nothing      -> pure (T.singleton c)
  where
    codeToCharMap =
      [('t', '\t'), ('b', '\b'), ('n', '\n'), ('r', '\r'), ('f', '\f')]

-- | [161s] WS
-- Optional whitepace or comment.
pWS :: TurtleParser ()
pWS = void $ many (pWS1' <|> pComment)

-- | Mandatory whitespace or comment.
pWS1 :: TurtleParser ()
pWS1 = void $ some (pWS1' <|> pComment)

-- | Whitespace subparser for pWS and pWS1.
pWS1' :: TurtleParser ()
pWS1' = void $ takeWhile1P (Just "whitespace") (`elem` [' ', '\t', '\r', '\n'])

-- | Parse a comment.
pComment :: TurtleParser ()
pComment = void $ char '#' *> takeWhileP (Just "comment") (`notElem` ['\r', '\n'])

-- [162s]
pAnon :: TurtleParser Term
pAnon = do
  _ <- char '[' <* pWS <* char ']' <?> "anonymous node"
  BNodeGen <$> getNextBlankNodeID

-- | [163s] PN_CHARS_BASE
isPNCharsBase :: Char -> Bool
isPNCharsBase c = isAsciiUpper c
    || isAsciiLower c
    || (c >= '\x00c0' && c <= '\x00d6')
    || (c >= '\x00c0' && c <= '\x00d6')
    || (c >= '\x00d8' && c <= '\x00f6')
    || (c >= '\x00f8' && c <= '\x02ff')
    || (c >= '\x0370' && c <= '\x037d')
    || (c >= '\x037f' && c <= '\x1fff')
    || (c >= '\x200c' && c <= '\x200d')
    || (c >= '\x2070' && c <= '\x218f')
    || (c >= '\x2c00' && c <= '\x2fef')
    || (c >= '\x3001' && c <= '\xd7ff')
    || (c >= '\xf900' && c <= '\xfdcf')
    || (c >= '\xfdf0' && c <= '\xfffd')

-- | [164s] PN_CHARS_U
isPNCharsU :: Char -> Bool
isPNCharsU c = isPNCharsBase c || c == '_'

-- | [166s] PN_CHARS
isPNChars :: Char -> Bool
isPNChars c = isPNCharsU c
    || c == '-'
    || isDigit c
    || c == '\x00b7'
    || (c >= '\x0300' && c <= '\x036f')
    || (c >= '\x203f' && c <= '\x2040')

-- [167s]
-- TODO remove partial head and last
pPNPrefix :: TurtleParser T.Text
pPNPrefix = do
  p <- TL.toStrict <$> takeWhile1P Nothing isPNChars
  let hd = T.head p
      lt = T.last p
  if isPNCharsBase hd
    then if isPNChars lt
      then pure p
      else customFailure $ "prefix may not end with '" <> (T.singleton lt) <> "'."
    else customFailure $ "prefix may not start with '" <> (T.singleton hd) <> "'."

-- [168s] PN_LOCAL
-- TODO remove partial head and last
pPNLocal :: TurtleParser T.Text
pPNLocal = do
  p <- mconcat <$> many (
      takeWhile1P Nothing (\c -> isPNChars c || c == '.' || c == ':') <|> pPLX)
  let hd = TL.head p
      lt = TL.last p
  if isPNCharsU hd || hd == ':' || isDigit hd || hd == '%' || hd == '\\'
    then if isPNChars lt || lt == ':' || hd == '%' || hd == '\\'
      then pure (TL.toStrict p)
      else customFailure $ "PN_LOCAL may not end with '" <> (T.singleton lt) <> "'."
    else customFailure $ "PN_LOCAL may not start with '" <> (T.singleton hd) <> "'."

-- | [169s] PLX (Note: this should not be decoded by the parser!)
pPLX :: TurtleParser TL.Text
pPLX = pPercent <|> pPNLocalEsc

-- | [170s] PERCENT
pPercent :: TurtleParser TL.Text
pPercent = TL.pack <$> sequence [char '%', hexDigitChar, hexDigitChar]

-- | [171s] PN_LOCAL_ESC
pPNLocalEsc :: TurtleParser TL.Text
pPNLocalEsc = TL.pack <$> sequence [char '\\', oneOf allowedChars]
  where allowedChars = [
          '_', '~', '.', '-', '!', '$', '&', '\'', '(', ')',
          '*', '+', ',', ';', '=', '/', '?', '#', '@', '%']
