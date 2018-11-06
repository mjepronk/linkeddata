-- | Parsers common to both the NTriples and Turtle parser

{-# LANGUAGE OverloadedStrings #-}

module LinkedData.Serialisation.Common
 ( pStringLiteralQuote
 , pUChar
 , pEChar
 , pBlankNodeLabel
 , pLangTag
 , isPNCharsBase
 , isPNCharsU
 , isPNChars
 )
where

import           Control.Applicative ((<|>), many)
import           Data.Char (chr, digitToInt, isAsciiLower, isAsciiUpper, isDigit, isHexDigit)
import           Data.Foldable (find, foldl')
import qualified Data.Text as T
import           Data.Attoparsec.Combinator (lookAhead, count)
import           Data.Attoparsec.Text (Parser, takeWhile1, try, char, string, satisfy)

import           LinkedData.Types


-- | NTriples: [9] STRING_LITERAL_QUOTE
-- | Turtle: [22] STRING_LITERAL_QUOTE
pStringLiteralQuote :: Parser T.Text
pStringLiteralQuote = (char '"') *> p <* (char '"')
  where p = mconcat <$> many (takeWhile1 (`notElem` ['"', '\\', '\n', '\r']) <|> try pEChar <|> pUChar)

-- | NTriples: [10] UCHAR
-- Turtle: [26] UCHAR
pUChar :: Parser T.Text
pUChar = do
    uchar <- try uchar16 <|> uchar32
    pure $ T.singleton (chr (hexToInt uchar))
  where
    uchar16 :: Parser String
    uchar16 = char '\\' *> char 'u' *> count 4 (satisfy isHexDigit)

    uchar32 :: Parser String
    uchar32 = char '\\' *> char 'U' *> count 8 (satisfy isHexDigit)

    hexToInt :: String -> Int
    hexToInt = foldl' ((+) . (16 *)) 0 . map digitToInt

-- | NTriples: [153s] ECHAR
-- Turtle: [159s] ECHAR
pEChar :: Parser T.Text
pEChar = do
    _ <- char '\\'
    c <- satisfy (`elem` ['t', 'b', 'n', 'r', 'f', '"', '\'', '\\'])
    case find (\(f, _) -> f == c) codeToCharMap of
      Just (_, t') -> pure (T.singleton t')
      Nothing      -> pure (T.singleton c)
  where
    codeToCharMap =
      [('t', '\t'), ('b', '\b'), ('n', '\n'), ('r', '\r'), ('f', '\f')]

-- | [141s] BLANK_NODE_LABEL
-- TODO if blank is of form _:genidXXX return BNodeGen with new id instead of BNode.
pBlankNodeLabel :: Parser Term
pBlankNodeLabel = do
    _ <- string "_:"
    x <- T.singleton <$> satisfy (\c -> isPNCharsU c || isDigit c)
    xs <- mconcat <$> many (
      pn_chars <|> (T.singleton <$> char '.' <* lookAhead pn_chars))
    pure (BNode $ x <> xs)
  where
    pn_chars = takeWhile1 isPNChars

-- | [144s] LANG_TAG
pLangTag :: Parser T.Text
pLangTag = do
    _ <- char '@'
    x <- T.singleton <$> satisfy (\c -> isAsciiLower c || isAsciiUpper c)
    xs <- mconcat <$> many (
      langChar <|> (T.singleton <$> char '-' <* lookAhead langChar))
    pure (x <> xs)
  where
    langChar = takeWhile1 (\c -> isAsciiLower c || isAsciiUpper c || isDigit c)

-- | NTriples: [157s] PN_CHARS_BASE
-- Turtle: [163s] PN_CHARS_BASE
isPNCharsBase :: Char -> Bool
isPNCharsBase c = isAsciiUpper c
    || isAsciiLower c
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
    || (c >= '\x10000' && c <= '\xeffff')

-- | NTriples: [158s] PN_CHARS_U
-- Turtle: [164s] PN_CHARS_U
isPNCharsU :: Char -> Bool
isPNCharsU c = isPNCharsBase c || c == '_'

-- | NTriples: [160s] PN_CHARS
-- Turtle: [166s] PN_CHARS
isPNChars :: Char -> Bool
isPNChars c = isPNCharsU c
    || c == '-'
    || isDigit c
    || c == '\x00b7'
    || (c >= '\x0300' && c <= '\x036f')
    || (c >= '\x203f' && c <= '\x2040')
