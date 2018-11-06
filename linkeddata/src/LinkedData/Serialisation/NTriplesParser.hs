-- | N-Triples parser
--
-- See:
-- - https://www.w3.org/TR/n-triples/#n-triples-grammar
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


module LinkedData.Serialisation.NTriplesParser
  ( parseNTriples
  , parseNTriplesFile
  )
where

import Prelude hiding (takeWhile)

import           Data.Functor (($>))
import           Data.Maybe (Maybe(..))
import           Data.Monoid ((<>))
import           Control.Applicative ((<|>))
import           Control.Monad (void)
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Text as T
import           Data.Attoparsec.Text (Parser, (<?>), takeWhile, takeWhile1, char, string)
import qualified Data.ByteString.Streaming as B
import           Streaming (Stream, Of, lift)
import           Text.Parser.Combinators (between, many, optional, sepEndBy)

import           LinkedData.Types
import           LinkedData.IRI (mkAbsIRI)
import           LinkedData.Serialisation.Common (pStringLiteralQuote,
                     pUChar, pLangTag, pBlankNodeLabel)
import           LinkedData.Serialisation.Streaming (parsed, decodeByteString,
                     decodeUtf8Pure, handleDecodeError, handleParserError)


-- | Consume strict 'T.Text' values in N-Triples format and yield parsed
-- triples. When a parsing or decoding error occurs a Left SerialisationError is
-- returned.
parseNTriples
  :: Monad m
  => Stream (Of T.Text) m r
  -> Stream (Of Triple) m (Either SerialisationError ())
parseNTriples s = parseNTriples' (s $> Right ())


-- | Read an N-Triples file and yield parsed triples. When a parsing or decoding
-- error occurs a Left SerialisationError is returned.
parseNTriplesFile
  :: MonadResource m
  => FilePath
  -> Stream (Of Triple) m (Either SerialisationError ())
parseNTriplesFile = parseNTriples' . readFile'
  where
    readFile' fp = decodeByteString decodeUtf8Pure (B.readFile fp) >>= handleDecodeError


-- | Internal helper for parseNTriples and parseNTriplesFile.
parseNTriples'
  :: Monad m
  => Stream (Of T.Text) m (Either SerialisationError ())
  -> Stream (Of Triple) m (Either SerialisationError ())
parseNTriples' s = parsed (pWS *> pTriple <* pEOL) s >>= \case
    err@(Left _) -> lift $ handleParserError err
    Right r -> pure r  -- return value from s


-- [1] ntriplesDoc
pNTriplesDoc :: Parser Graph
pNTriplesDoc = do
  ts <- pWS *> sepEndBy pTriple (pEOL <* pWS)
  pure emptyGraph { triples = ts }

-- [2] triple
pTriple :: Parser Triple
pTriple = do
  s <- pSubject   <* pWS
  p <- pPredicate <* pWS
  o <- pObject    <* pWS
  _ <- char '.'   <* pWS
  pure (Triple s p o)

-- | [3] subject
pSubject :: Parser Term
pSubject = pIRIRef <|> pBlankNodeLabel <?> "subject"

-- | [4] predicate
pPredicate :: Parser Term
pPredicate = pIRIRef <?> "predicate"

-- | [5] object
pObject :: Parser Term
pObject = pIRIRef <|> pBlankNodeLabel <|> pLiteral <?> "object"

-- | [6] literal
pLiteral :: Parser Term
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

-- | [7] EOL
pEOL :: Parser ()
pEOL = void $ takeWhile1 (`elem` ['\r', '\n'])

-- | Skip zero or more whitepace characters.
pWS :: Parser ()
pWS = void $ pWS' *> optional pComment
  where
    pWS' = void $ takeWhile (`elem` [' ', '\t'])

-- | Parse a comment.
pComment :: Parser ()
pComment = void $ char '#' *> takeWhile (`notElem` ['\r', '\n'])

-- | [8] IRIREF
pIRIRef :: Parser Term
pIRIRef = do
    iri <- between (char '<') (char '>') pIRI'
    case mkAbsIRI iri of
      Nothing -> fail $ "Could not parse IRI '" <> T.unpack iri <> "'."
      Just iri' -> pure (ITerm iri')
  where
    pIRI' = mconcat <$> many (pIRIChar <|> pUChar)
    pIRIChar = takeWhile1 (\c -> c > '\x0020' &&
                                 c `notElem` ['<', '>', '"', '{', '}', '|', '^', '`', '\\'])
