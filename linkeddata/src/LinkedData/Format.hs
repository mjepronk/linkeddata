{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

-- The functions showDecimal and showDecimal' are from HXT:
-- hxt/hxt-xmlschema/src/Text/XML/HXT/XMLSchema/W3CDataTypeCheck.hs

module LinkedData.Format
    (
    -- * Formatters for RDF values
      formatBool
    , formatRealFloat
    , formatRational
    , formatInteger
    , formatDateTime
    , formatDate
    , formatTime

    -- * Parsers for RDF values
    , parseBool
    , parseRealFloat
    , parseRational
    , parseInteger
    , parseDateTime
    , parseDate
    , parseTime
    )
where

import           Control.Applicative ((<|>))
import           Control.Monad (when, unless)
import           Control.Monad.Except (MonadError, throwError)
import           GHC.Real (infinity, notANumber)
import           Data.Char (isDigit)
import           Data.Fixed (Pico)
import           Data.Maybe (isJust)
import           Data.Ratio (numerator, denominator)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar (Day, fromGregorianValid)
import           Data.Time.LocalTime (TimeZone, TimeOfDay, LocalTime(..),
                     ZonedTime(..), utc, makeTimeOfDayValid, minutesToTimeZone,
                     zonedTimeToUTC)
import qualified Data.Time.Format as TF
import           Data.Attoparsec.Combinator (lookAhead)
import           Data.Attoparsec.Text (Parser, (<?>), parseOnly, takeWhile1,
                     double, decimal, signed, char, option, rational, endOfInput)
import           Text.Parser.Char (oneOf)
import           Text.Parser.Combinators (optional)

-- | Format a 'Bool' to an xsd:boolean value.
formatBool :: Bool -> Text
formatBool True  = "true"
formatBool False = "false"

-- | Format a 'Float' or 'Double' to an xsd:double or xsd:float value.
formatRealFloat :: (Show a, RealFloat a) => a -> Text
formatRealFloat x
    | isNaN x               = "NaN"
    | isInfinite x && x < 0 = "-INF"
    | isInfinite x          = "INF"
    | otherwise             = T.pack (show x)

-- | Format a 'Rational' to an xsd:decimal value.
formatRational :: Rational -> Text
formatRational rat = T.pack $  (if num < 0 then "-" else "") ++ (shows d ("." ++ take len (go next)))
  where
    (d, next) = abs num `quotRem` den
    num = numerator rat
    den = denominator rat
    len = 100

    go 0 = ""
    go x = let (d', next') = (10 * x) `quotRem` den
           in shows d' (go next')

-- | Format an 'Integral' to an xsd:integer value.
formatInteger :: Integer -> Text
formatInteger = T.pack . show

-- | Format a 'LocalTime' and optional 'TimeZone' to an xsd:dateTime value.
-- See: https://www.w3.org/TR/xmlschema-2/#dateTime-canonical-representation
formatDateTime :: LocalTime -> Maybe TimeZone -> Text
formatDateTime lt z =
  case z of
    Just tz ->
      let lt' = zonedTimeToUTC (ZonedTime lt tz)
      in  T.pack (TF.formatTime TF.defaultTimeLocale "%0Y-%m-%dT%H:%M:%S%QZ" lt')
    Nothing ->
      T.pack (TF.formatTime TF.defaultTimeLocale "%0Y-%m-%dT%H:%M:%S%Q" lt)

-- | Format a 'Day' to an xsd:date value.
-- See: https://www.w3.org/TR/xmlschema-2/#date-canonical-representation
formatDate :: Day -> Text
formatDate = T.pack . TF.formatTime TF.defaultTimeLocale "%0Y-%m-%d"

-- | Format a 'TimeOfDay' to an xsd:time value.
-- See: https://www.w3.org/TR/xmlschema-2/#time-canonical-repr
formatTime :: TimeOfDay -> Maybe TimeZone -> Text
formatTime = undefined

-- | Parse an xsd:boolean to a 'Bool' value.
parseBool :: MonadError String m => Text -> m Bool
parseBool "true"  = pure True
parseBool "false" = pure False
parseBool "1"     = pure True
parseBool "0"     = pure False
parseBool invalid = throwError ("Invalid boolean value: " <> T.unpack invalid <> ".")

-- | Parse an xsd:float or xsd:double to a 'Float' or 'Double' value.
parseRealFloat :: (MonadError String m, RealFloat a) => Text -> m a
parseRealFloat "INF"  = pure . realToFrac $ infinity
parseRealFloat "-INF" = pure . realToFrac . negate $ infinity
parseRealFloat "NaN"  = pure . realToFrac $ notANumber
parseRealFloat x =
    case parseOnly pRealFloat x of
      Right x' -> pure x'
      Left err -> throwError err
  where
    pRealFloat = do
      d <- signed double
      endOfInput
      pure (realToFrac d)

-- | Parse an xsd:decimal value to a 'Rational' value.
parseRational :: MonadError String m => Text -> m Rational
parseRational x =
    case parseOnly pRational x of
      Right x' -> pure x'
      Left err -> throwError err
  where
    pRational = checkRational *> signed rational <* endOfInput
    checkRational =
      lookAhead $ do
        _ <- takeWhile1 (\c -> isDigit c || c `elem` ['+', '-', '.']) <?>
          "Invalid decimal, must contain a sign, digits and decimal separator"
        endOfInput <?>
          "Invalid decimal, may only contain sign, digits and decimal seperator"

-- | Parse xsd:integer to an 'Integer' value.
parseInteger :: (MonadError String m) => Text -> m Integer
parseInteger x =
    case parseOnly pInteger x of
      Right x' -> pure x'
      Left err -> throwError err
  where
    pInteger = do
      d <- signed decimal
      endOfInput
      pure d

-- | Parse xsd:datetime to a 'DateTime' value.
parseDateTime :: (MonadError String m) => Text -> m (LocalTime, Maybe TimeZone)
parseDateTime x =
    case parseOnly (pDateTime <* endOfInput) x of
      Right x' -> pure x'
      Left err -> throwError err
  where
    pDateTime :: Parser (LocalTime, Maybe TimeZone)
    pDateTime = do
      d <- pDay
      _ <- char 'T'
      t <- pTimeOfDay
      z <- optional pTimeZone
      pure (LocalTime d t, z)

-- | Parse xsd:date to a 'Day' value.
parseDate :: (MonadError String m) => Text -> m Day
parseDate x =
  case parseOnly (pDay <* endOfInput) x of
    Right x' -> pure x'
    Left err -> throwError err

-- | Parse xsd:time to a 'Time' value.
parseTime :: (MonadError String m) => Text -> m (TimeOfDay, Maybe TimeZone)
parseTime x =
  case parseOnly ((,) <$> pTimeOfDay <*> optional pTimeZone <* endOfInput) x of
    Right x' -> pure x'
    Left err -> throwError err

pDay :: Parser Day
pDay = do
    s <- optional (char '-')
    y <- checkYear *> decimal <* char '-'
    m <- decimal <* char '-'
    d <- decimal
    let y' = if isJust s then negate y else y
    case fromGregorianValid y' m d of
      Just x  -> pure x
      Nothing -> fail ("invalid date " <> show y' <> "-" <> show m <> "-" <> show d)
  where
    checkYear =
      lookAhead $ do
        y' <- takeWhile1 isDigit
        let len = T.length y'
        unless (len >= 4) $ fail "Invalid year, must have at least 4 digits"
        when (len > 4 && "0" `T.isPrefixOf` y') $
           fail "Invalid year, may not start with a 0 when more than 4 digits"
        unless (y' /= "0000") $ fail "Invalid year, may not be 0000"

pTimeOfDay :: Parser TimeOfDay
pTimeOfDay = do
    h <- decimal <* char ':'
    m <- decimal <* char ':'
    s <- takeWhile1 isDigit
    f <- option "0" (char '.' *> takeWhile1 isDigit)
    let s' = read (T.unpack s <> "." <> T.unpack f) :: Pico
    case makeTimeOfDayValid h m s' of
      Just x  -> pure x
      Nothing -> fail ("Invalid time: " <> show h <> ":" <> show m <> ":" <> show s')

pTimeZone :: Parser TimeZone
pTimeZone = pTZ <|> pUTC
  where
    pTZ = do
      s <- oneOf "+-"
      h <- decimal <* char ':'
      m <- decimal
      case s of
        '-' -> pure (minutesToTimeZone (negate ((h * 60) + m)))
        _   -> pure (minutesToTimeZone ((h * 60) + m))
    pUTC = char 'Z' *> pure utc
