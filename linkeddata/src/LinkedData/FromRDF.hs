--
-- See:
-- - https://www.w3.org/TR/xmlschema11-2/#built-in-primitive-datatypes
--
-- TODO:
-- - Read up on Aeson's implementation, it's much better
-- - Provide function to convert a Collection to a List
--

{-# LANGUAGE FlexibleInstances #-}

module LinkedData.FromRDF
  ( FromRDF(..)
  , isNumericType
  , isDerivedByRestriction
  )
where

import           Data.Maybe (Maybe(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Time.Calendar (Day)
import           Data.Time.LocalTime (LocalTime(..), ZonedTime(..))
import           Numeric.Natural (Natural)

import           LinkedData.Format (parseBool, parseRealFloat, parseRational,
                     parseInteger, parseDate, parseDateTime)
import           LinkedData.Types (LValue(..), Term(..))
import           LinkedData.IRI (IRI, Abs, unIRI)
import           LinkedData.Namespaces (xsdAnyType, xsdAnySimpleType, xsdString,
                     xsdBoolean, xsdFloat, xsdDateTime, xsdDate, xsdDecimal,
                     xsdDouble, xsdNormalizedString, xsdInteger, xsdToken,
                     xsdNonPositiveInteger, xsdLong, xsdNonNegativeInteger,
                     xsdNegativeInteger, xsdInt, xsdUnsignedLong,
                     xsdPositiveInteger, xsdShort, xsdUnsignedInt,
                     xsdByte, xsdUnsignedShort, xsdUnsignedByte)


class FromRDF a where
  fromLiteral :: LValue -> Maybe a
  fromRDF :: Term -> Maybe a

  fromRDF (LTerm l) = fromLiteral l
  fromRDF _ = Nothing

instance FromRDF Bool where
  fromLiteral (TypedL x t)
    | t == xsdBoolean = rightToMaybe (parseBool x)
    | otherwise = Nothing
  fromLiteral _ = Nothing

instance FromRDF Integer where
  fromLiteral (PlainL x) = rightToMaybe (parseInteger x)
  fromLiteral (TypedL x t)
    | t `isDerivedByRestriction` xsdInteger = rightToMaybe (parseInteger x)
    | otherwise = Nothing
  fromLiteral _ = Nothing

instance FromRDF Int where
  fromLiteral x = fromIntegral <$> (fromLiteral x :: Maybe Integer)

instance FromRDF Natural where
  fromLiteral x = fromIntegral <$> (fromLiteral x :: Maybe Integer)

instance FromRDF Rational where
  fromLiteral (PlainL x) = rightToMaybe (parseRational x)
  fromLiteral (TypedL x t)
    | isNumericType t = rightToMaybe (parseRational x)
    | otherwise = Nothing
  fromLiteral _ = Nothing

instance FromRDF Float where
  fromLiteral (PlainL x) = rightToMaybe (parseRealFloat x)
  fromLiteral (TypedL x t)
    | isNumericType t = rightToMaybe (parseRealFloat x)
    | otherwise = Nothing
  fromLiteral _ = Nothing

instance FromRDF Double where
  fromLiteral (PlainL x) = rightToMaybe (parseRealFloat x)
  fromLiteral (TypedL x t)
    | isNumericType t = rightToMaybe (parseRealFloat x)
    | otherwise = Nothing
  fromLiteral _ = Nothing

instance FromRDF T.Text where
  fromLiteral (PlainL x)    = pure x
  fromLiteral (PlainLL x _) = pure x
  fromLiteral (TypedL x _)  = pure x

  fromRDF (ITerm iri) = pure (unIRI iri)
  fromRDF (LTerm l) = fromLiteral l
  fromRDF _ = Nothing

instance FromRDF TL.Text where
  fromLiteral x = TL.fromStrict <$> fromLiteral x

  fromRDF x = TL.fromStrict <$> fromRDF x

instance FromRDF String where
  fromLiteral x = T.unpack <$> fromLiteral x

  fromRDF x = T.unpack <$> fromRDF x

instance FromRDF Day where
  fromLiteral (TypedL x ty)
    | ty == xsdDateTime = do
        (d, _) <- rightToMaybe (parseDateTime x)
        pure (localDay d)
    | ty == xsdDate = rightToMaybe (parseDate x)
    | otherwise = Nothing
  fromLiteral _ = Nothing

-- instance FromRDF TimeOfDay where
--   fromLiteral (TypedL x ty)
--     | ty == xsdDateTime = do
--         (d, _) <- rightToMaybe (parseDateTime x)
--         pure (localTimeOfDay d)
--     | otherwise = Nothing
--   fromLiteral _ = Nothing

instance FromRDF LocalTime where
  fromLiteral (TypedL x ty)
    | ty == xsdDateTime = do
        (d, _) <- rightToMaybe (parseDateTime x)
        pure d
    | otherwise = Nothing
  fromLiteral _ = Nothing

instance FromRDF ZonedTime where
  fromLiteral (TypedL x ty)
    | ty == xsdDateTime = do
        (d, t) <- rightToMaybe (parseDateTime x)
        case t of
          Just tz -> pure (ZonedTime d tz)
          Nothing -> Nothing
    | otherwise = Nothing
  fromLiteral _ = Nothing

-- instance FromRDF (LocalTime, Maybe TimeZone) where
--   fromLiteral (TypedL x ty)
--     | ty == xsdDateTime = do
--         (d, t) <- rightToMaybe (parseDateTime x)
--         pure (d, t)
--     | otherwise = Nothing
--   fromLiteral _ = Nothing

instance FromRDF (IRI Abs) where
  fromLiteral _ = Nothing

  fromRDF (ITerm x) = pure x
  fromRDF _ = Nothing

instance FromRDF Term where
  fromLiteral x = pure (LTerm x)

  fromRDF = Just


-- | Is the term numeric as defined in XML Schema Part 2.
-- See: https://www.w3.org/TR/xmlschema-2/
isNumericType :: IRI Abs -> Bool
isNumericType t =
       t `isDerivedByRestriction` xsdFloat
    || t `isDerivedByRestriction` xsdDouble
    || t `isDerivedByRestriction` xsdDecimal

-- | Test if a type is derived from another base type (as defined in XML Schema
-- Part 2).
-- See: https://www.w3.org/TR/xmlschema-2/#built-in-datatypes
isDerivedByRestriction :: IRI Abs -- ^ type to test
                       -> IRI Abs -- ^ base type
                       -> Bool
isDerivedByRestriction t base =
       t == base
    -- level 0
    || base == xsdAnyType
    -- level 1
    || xsdAnySimpleType      `isDerivedFrom'` xsdAnyType
    -- level 2
    || xsdString             `isDerivedFrom'` xsdAnySimpleType
    || xsdBoolean            `isDerivedFrom'` xsdAnySimpleType
    || xsdFloat              `isDerivedFrom'` xsdAnySimpleType
    || xsdDecimal            `isDerivedFrom'` xsdAnySimpleType
    || xsdDouble             `isDerivedFrom'` xsdAnySimpleType
    -- level 3
    || xsdNormalizedString   `isDerivedFrom'` xsdString
    || xsdInteger            `isDerivedFrom'` xsdDecimal
    -- level 4
    || xsdToken              `isDerivedFrom'` xsdNormalizedString
    || xsdNonPositiveInteger `isDerivedFrom'` xsdInteger
    || xsdLong               `isDerivedFrom'` xsdInteger
    || xsdNonNegativeInteger `isDerivedFrom'` xsdInteger
    -- level 5
    || xsdNegativeInteger    `isDerivedFrom'` xsdNonPositiveInteger
    || xsdInt                `isDerivedFrom'` xsdLong
    || xsdUnsignedLong       `isDerivedFrom'` xsdNonNegativeInteger
    || xsdPositiveInteger    `isDerivedFrom'` xsdNonNegativeInteger
    -- level 6
    || xsdShort              `isDerivedFrom'` xsdInt
    || xsdUnsignedInt        `isDerivedFrom'` xsdUnsignedLong
    -- level 7
    || xsdByte               `isDerivedFrom'` xsdShort
    || xsdUnsignedShort      `isDerivedFrom'` xsdUnsignedInt
    -- level 8
    || xsdUnsignedByte       `isDerivedFrom'` xsdUnsignedShort
  where
    isDerivedFrom' :: IRI Abs  -- ^ ty, type we want to test against
                   -> IRI Abs  -- ^ dfty, "derived from" type of ty
                   -> Bool
    isDerivedFrom' ty dfty =
         t == ty && base == dfty
      || t == ty && dfty `isDerivedByRestriction` base


rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just
