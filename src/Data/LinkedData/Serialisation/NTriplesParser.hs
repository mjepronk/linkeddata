-- | N-Triples parser
--
-- See:
-- - https://www.w3.org/TR/n-triples/#n-triples-grammar
--

{-# LANGUAGE OverloadedStrings #-}

module Data.LinkedData.Serialisation.NTriplesParser
  ( parseNTriples
  , parseNTriplesFile )
where

import           Data.Char (digitToInt, chr, isDigit, isAsciiLower, isAsciiUpper)
import           Data.Foldable (find)
import           Data.Monoid ((<>))
import           Control.Monad (void)
import           Control.Applicative ((<|>))
import           Control.Applicative.Combinators (sepEndBy, between, count, optional)
import           Data.Functor.Identity (Identity)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Text.Megaparsec ((<?>), ParsecT, ParseError(..), parse,
                     try, customFailure, takeWhileP, takeWhile1P, eof)
import           Text.Megaparsec.Char (char, hexDigitChar, oneOf, string)
import           Data.LinkedData.Types
import qualified Data.LinkedData.Types as LD
import           Path (Path, File, toFilePath)


type NTriplesParser = ParsecT T.Text TL.Text Identity


parseNTriples :: Maybe (Path a File)
              -> TL.Text
              -> Either (ParseError Char T.Text) Graph
parseNTriples p = parse pNTriplesDoc (maybe "" toFilePath p)

parseNTriplesFile :: Path a File
                  -> IO (Either (ParseError Char T.Text) Graph)
parseNTriplesFile fp = parseNTriples (Just fp) . TL.decodeUtf8 <$>
                         BL.readFile (toFilePath fp)


-- [1] ntriplesDoc
pNTriplesDoc :: NTriplesParser Graph
pNTriplesDoc = do
  ts <- pWS *> sepEndBy pTriple (pEOL <* pWS)
  eof
  pure emptyGraph { triples = ts }

-- [2] triple
pTriple :: NTriplesParser Triple
pTriple = do
  s <- pSubject   <* pWS
  p <- pPredicate <* pWS
  o <- pObject    <* pWS
  _ <- char '.'   <* pWS
  case LD.triple s p o of
    Right t -> pure t
    Left  e -> customFailure $ (T.pack (show (s, p, o))) <> ": " <> e

-- [3] subject  TODO: is try necessary?
pSubject :: NTriplesParser Term
pSubject = try pIRIRef <|> pBlankNodeLabel <?> "subject"

-- [4] predicate
pPredicate :: NTriplesParser Term
pPredicate = pIRIRef <?> "predicate"

-- [5] object  TODO: is try necessary?
pObject :: NTriplesParser Term
pObject = try pIRIRef <|> try pBlankNodeLabel <|> pLiteral <?> "object"

-- [6] literal
pLiteral :: NTriplesParser Term
pLiteral = do
  s <- pStringLiteralQuote
  l <- optional pLangTag
  case l of
    Just l' -> pure (plainLL s l')
    Nothing -> do
      t <- optional (string "^^" *> pIRIRef)
      case t of
        Just (ITerm t') -> pure (typedL s t')
        _               -> pure (plainL s)

-- [144s] LANG_TAG
pLangTag :: NTriplesParser T.Text
pLangTag = do
    _ <- char '@'
    s <- TL.toStrict <$> takeWhileP (Just "language identifier") validChar
    if validFirstChar (T.head s)
      then if validLastChar (T.last s)
        then pure s
        else customFailure "language identifier may not end with a dash"
      else customFailure "language identifier must start with a letter"
  where
    validChar c = c == '-' || isAsciiLower c || isAsciiUpper c || isDigit c
    validFirstChar c = isAsciiLower c || isAsciiUpper c
    validLastChar c = c /= '-'

-- [7] EOL
pEOL :: NTriplesParser ()
pEOL = void $ takeWhile1P (Just "EOL") (`elem` ['\r', '\n'])

-- | Skip zero or more whitepace characters.
pWS :: NTriplesParser ()
pWS = void $ pWS' *> optional pComment
  where
    pWS' = void $ takeWhileP (Just "whitespace") (`elem` [' ', '\t'])

-- | Parse a comment.
pComment :: NTriplesParser ()
pComment = void $ char '#' *> takeWhileP (Just "comment") (`notElem` ['\r', '\n'])

-- [8] IRIREF
pIRIRef :: NTriplesParser Term
pIRIRef = ITerm . IRI <$> between (char '<') (char '>') iriChars
    -- TODO: T.concat <$> many (iriChars <|> pUChar)
  where
    iriChars = TL.toStrict <$> takeWhileP
      (Just "IRI char")
      ( \c -> c > '\x0020' && c `notElem` ['<', '>', '"', '{', '}', '|', '^', '`', '\\'])

-- [9] STRING_LITERAL_QUOTE
pStringLiteralQuote :: NTriplesParser T.Text
pStringLiteralQuote = between (char '"') (char '"') innerp
 where
  innerp   = litchars -- TODO: T.concat <$> many (litchars <|> pEChar <|> pUChar)
  litchars = TL.toStrict <$> takeWhileP (Just "literal char") (`notElem` ['"', '\\', '\n', '\r'])

-- [141s] BLANK_NODE_LABEL
pBlankNodeLabel :: NTriplesParser Term
pBlankNodeLabel = do
  _ <- string "_:"
  l <- TL.toStrict <$> takeWhile1P Nothing isPnChars
  let hd = T.head l
      lt = T.last l
  if isPnCharsU hd || isDigit hd
    then if isPnChars lt
      then pure (BNode l)
      else
        customFailure $ "blank node label may not end with: " <> (T.singleton lt)
    else
      customFailure $ "blank node label may not start with: " <> (T.singleton hd)

-- [10] UCHAR
pUChar :: NTriplesParser T.Text
pUChar = do
  uchar <- try uchar16 <|> uchar32
  pure $ T.singleton (chr (hexToInt uchar))
 where
  uchar16 :: NTriplesParser String
  uchar16 = char '\\' *> char 'u' *> count 4 hexDigitChar

  uchar32 :: NTriplesParser String
  uchar32 = char '\\' *> char 'U' *> count 8 hexDigitChar

  hexToInt :: String -> Int
  hexToInt = foldl ((+) . (16 *)) 0 . map digitToInt

-- [153s] ECHAR
pEChar :: NTriplesParser T.Text
pEChar = do
  _ <- char '\\'
  c <- oneOf ['t', 'b', 'n', 'r', 'f', '"', '\'', '\\']
  case find (\(f, _) -> f == c) codeToCharMap of
    Just (_, t') -> pure (T.singleton t')
    Nothing      -> pure (T.singleton c)
 where
  codeToCharMap =
    [('t', '\t'), ('b', '\b'), ('n', '\n'), ('r', '\r'), ('f', '\f')]

-- [157s] PN_CHARS_BASE
isPnCharsBase :: Char -> Bool
isPnCharsBase c = isAsciiUpper c
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

 -- [158s] PN_CHARS_U
isPnCharsU :: Char -> Bool
isPnCharsU c = isPnCharsBase c || c == '_'

-- [160s] PN_CHARS
isPnChars :: Char -> Bool
isPnChars c = isPnCharsU c
    || c == '-'
    || isDigit c
    || c == '\x00b7'
    || (c >= '\x0300' && c <= '\x036f')
    || (c >= '\x203f' && c <= '\x2040')

-- [162s] HEX ::= [0-9] | [A-F] | [a-f]
