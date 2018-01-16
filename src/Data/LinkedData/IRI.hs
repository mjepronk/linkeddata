{-# LANGUAGE OverloadedStrings #-}

module Data.LinkedData.IRI where

import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.LinkedData.Types (Term(..), IRI(..))
import qualified Data.Text as T
import qualified Text.URI as URI


(#) :: IRI -> T.Text -> IRI
b # r = (IRI r) `unsafeRelativeTo` b

-- TODO: implement this properly
relativeTo :: IRI -> IRI -> Maybe IRI
relativeTo ref@(IRI r) (IRI b) = do
   isAbs <- isAbsoluteIRI ref
   if isAbs
    then pure ref
    else pure $ IRI (b <> r)

-- Unsafe version of 'relativeTo'. Only use with hardcoded reference and base
-- IRI's that you are sure will not cause exceptions.
unsafeRelativeTo :: IRI -> IRI -> IRI
unsafeRelativeTo r b = fromJust $ r `relativeTo` b

-- TODO: implement this properly
relativeFrom :: IRI -> IRI -> Maybe IRI
relativeFrom (IRI r) (IRI b)
  | b `T.isPrefixOf` r = Just . IRI $ T.drop (T.length b) r
  | otherwise          = Nothing

isBaseIRIOf :: IRI -> IRI -> Bool
isBaseIRIOf (IRI b) (IRI r) = b `T.isPrefixOf` r

isAbsoluteIRI :: IRI -> Maybe Bool
isAbsoluteIRI (IRI i) = do
    u <- URI.mkURI i
    pure (URI.isPathAbsolute u)

-- | TODO: replace this with lenses?
unIRI :: IRI -> T.Text
unIRI (IRI iri) = iri
