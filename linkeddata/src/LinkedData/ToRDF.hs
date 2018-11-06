-- | Convert Haskell values to RDF Terms.
--
-- See:
-- - https://www.w3.org/TR/xmlschema11-2/#built-in-primitive-datatypes
--
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module LinkedData.ToRDF
  ( ToRDF(..)
  )
where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Time.Calendar (Day)
import           Data.Time.LocalTime (LocalTime, ZonedTime(..))
import           Numeric.Natural (Natural)

import           LinkedData.Format (formatBool, formatRealFloat, formatRational,
                     formatInteger, formatDate, formatDateTime)
import           LinkedData.Types (Term(..), LValue(..))
import           LinkedData.Namespaces (xsdBoolean, xsdDecimal, xsdInteger,
                     xsdFloat, xsdDouble, xsdDate, xsdDateTime)


class ToRDF a where
    toRDF :: a -> Term
    toLiteral :: a -> LValue

    toRDF = LTerm . toLiteral

instance ToRDF Bool where
    toLiteral x = TypedL (formatBool x) xsdBoolean

instance ToRDF Int where
    toLiteral x = TypedL (formatInteger (fromIntegral x)) xsdInteger

instance ToRDF Integer where
    toLiteral x = TypedL (formatInteger x) xsdInteger

instance ToRDF Natural where
    toLiteral = toLiteral . toInteger

instance ToRDF Float where
    toLiteral x = TypedL (formatRealFloat x) xsdFloat

instance ToRDF Double where
    toLiteral x = TypedL (formatRealFloat x) xsdDouble

instance ToRDF Rational where
    toLiteral x = TypedL (formatRational x) xsdDecimal

instance ToRDF String where
    toLiteral = PlainL . T.pack

instance ToRDF T.Text where
    toLiteral = PlainL

instance ToRDF TL.Text where
    toLiteral = PlainL . TL.toStrict

instance ToRDF Day where
    toLiteral x = TypedL (formatDate x) xsdDate

instance ToRDF LocalTime where
    toLiteral x = TypedL (formatDateTime x Nothing) xsdDateTime

instance ToRDF ZonedTime where
    toLiteral x = TypedL (formatDateTime lt tz) xsdDateTime
      where
        lt = zonedTimeToLocalTime x
        tz = Just (zonedTimeZone x)

-- instance ToRDF (IRI Abs) where
--     toLiteral = Nothing
--     toRDF = ITerm

-- instance ToRDF Term where
--     toRDF = id
