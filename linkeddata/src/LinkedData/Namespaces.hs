{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module LinkedData.Namespaces
  (
    -- * Common RDF namespaces.
    commonNamespaces
  , owlNS
  , rdfNS
  , rdfsNS
  , rdfaNS
  , xmlNS
  , xsdNS
  , foafNS
  , skosNS
  , dcNS
  , dctNS

    -- * Common RDF IRI's
  , rdfType
  , rdfFirst
  , rdfRest
  , rdfNil
  , rdfLangString

    -- * Common XML Schema IRI's
  , xsdAnyType
  , xsdAnySimpleType

  , xsdString
  , xsdBoolean
  , xsdDecimal
  , xsdFloat
  , xsdDouble
  , xsdDateTime
  , xsdTime
  , xsdDate

  , xsdByte
  , xsdInt
  , xsdInteger
  , xsdLong
  , xsdNegativeInteger
  , xsdNonNegativeInteger
  , xsdNonPositiveInteger
  , xsdNormalizedString
  , xsdPositiveInteger
  , xsdShort
  , xsdToken
  , xsdUnsignedByte
  , xsdUnsignedInt
  , xsdUnsignedLong
  , xsdUnsignedShort
  )

where

import qualified Data.Map as M
import           LinkedData.IRI (IRI, Abs, Namespaces, Prefix(..), (.:.))
import           LinkedData.QQ (absiri, reliri)


owlNS, rdfNS, rdfsNS, rdfaNS, xmlNS, xsdNS, foafNS, skosNS, dcNS, dctNS :: IRI Abs
owlNS  = [absiri|http://www.w3.org/2002/07/owl#|]
rdfNS  = [absiri|http://www.w3.org/1999/02/22-rdf-syntax-ns#|]
rdfsNS = [absiri|http://www.w3.org/2000/01/rdf-schema#|]
rdfaNS = [absiri|http://www.w3.org/ns/rdfa#|]
xmlNS  = [absiri|http://www.w3.org/XML/1998/namespace|]
xsdNS  = [absiri|http://www.w3.org/2001/XMLSchema#|]
foafNS = [absiri|http://xmlns.com/foaf/0.1/|]
skosNS = [absiri|http://www.w3.org/2004/02/skos/core#|]
dcNS   = [absiri|http://purl.org/dc/elements/1.1/|]
dctNS  = [absiri|http://purl.org/dc/terms/|]

commonNamespaces :: Namespaces
commonNamespaces = M.fromList [
    (Prefix "owl",  owlNS)
  , (Prefix "rdf",  rdfNS)
  , (Prefix "rdfs", rdfsNS)
  , (Prefix "rdfa", rdfaNS)
  , (Prefix "xml",  xmlNS)
  , (Prefix "xsd",  xsdNS)
  , (Prefix "foaf", foafNS)
  , (Prefix "skos", skosNS)
  , (Prefix "dc",   dcNS)
  , (Prefix "dct",  dctNS)
  ]

rdfType, rdfFirst, rdfRest, rdfNil :: IRI Abs
rdfType  = rdfNS .:. [reliri|type|]
rdfFirst = rdfNS .:. [reliri|first|]
rdfRest  = rdfNS .:. [reliri|rest|]
rdfNil   = rdfNS .:. [reliri|nil|]

rdfLangString :: IRI Abs
rdfLangString = rdfNS .:. [reliri|langString|]
rdfHTML       = rdfNS .:. [reliri|HTML|]
rdfXMLLiteral = rdfNS .:. [reliri|XMLLiteral|]

xsdAnyType, xsdAnySimpleType :: IRI Abs
xsdAnyType       = xsdNS .:. [reliri||]
xsdAnySimpleType = xsdNS .:. [reliri|anySimpleType|]

-- * Primitive datatypes
-- See:
-- - https://www.w3.org/TR/rdf11-concepts/#section-Datatypes
-- - https://www.w3.org/TR/xmlschema-2/#built-in-primitive-datatypes
xsdString, xsdBoolean, xsdDecimal, xsdFloat, xsdDouble :: IRI Abs
xsdString   = xsdNS .:. [reliri|string|]
xsdBoolean  = xsdNS .:. [reliri|boolean|]
xsdDecimal  = xsdNS .:. [reliri|decimal|]
xsdFloat    = xsdNS .:. [reliri|float|]
xsdDouble   = xsdNS .:. [reliri|double|]
xsdDateTime, xsdTime, xsdDate :: IRI Abs
xsdDateTime = xsdNS .:. [reliri|dateTime|]
xsdTime     = xsdNS .:. [reliri|time|]
xsdDate     = xsdNS .:. [reliri|date|]

-- * Derived datatypes
-- See:
-- - https://www.w3.org/TR/rdf11-concepts/#section-Datatypes
-- - https://www.w3.org/TR/xmlschema-2/#built-in-derived
xsdByte, xsdInt, xsdInteger, xsdLong, xsdNegativeInteger :: IRI Abs
xsdByte               = xsdNS .:. [reliri|byte|]
xsdInt                = xsdNS .:. [reliri|int|]
xsdInteger            = xsdNS .:. [reliri|integer|]
xsdLong               = xsdNS .:. [reliri|long|]
xsdNegativeInteger    = xsdNS .:. [reliri|negativeInteger|]
xsdNonNegativeInteger, xsdNonPositiveInteger, xsdNormalizedString :: IRI Abs
xsdNonNegativeInteger = xsdNS .:. [reliri|nonNegativeInteger|]
xsdNonPositiveInteger = xsdNS .:. [reliri|nonPositiveInteger|]
xsdNormalizedString   = xsdNS .:. [reliri|normalizedString|]
xsdPositiveInteger, xsdShort, xsdToken, xsdUnsignedByte :: IRI Abs
xsdPositiveInteger    = xsdNS .:. [reliri|positiveInteger|]
xsdShort              = xsdNS .:. [reliri|short|]
xsdToken              = xsdNS .:. [reliri|token|]
xsdUnsignedByte       = xsdNS .:. [reliri|unsignedByte|]
xsdUnsignedInt, xsdUnsignedLong, xsdUnsignedShort :: IRI Abs
xsdUnsignedInt        = xsdNS .:. [reliri|unsignedInt|]
xsdUnsignedLong       = xsdNS .:. [reliri|unsignedLong|]
xsdUnsignedShort      = xsdNS .:. [reliri|unsignedShort|]
