{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}

-- TODO:
-- - Read up on Aeson's implementation, it's much better
-- - Provide function to convert a Collection to a List

module LinkedData.FromRDF
  ( FromRDF(..)
  )
where

import Data.Maybe (Maybe(..))
import qualified Data.Text as T

import LinkedData.Types
import LinkedData.IRI (IRI, Abs, (.:.), unIRI)
import LinkedData.QQ (reliri)


class FromRDF a where
  fromRDF :: Term -> Maybe a


instance FromRDF Int where
  fromRDF (LTerm (PlainL x)) = pure . read . T.unpack $ x
  fromRDF (LTerm (TypedL x t))
    | t == xsdNS .:. [reliri|integer|] = pure . read . T.unpack $ x
    | otherwise = Nothing
  fromRDF _ = Nothing

instance FromRDF T.Text where
  fromRDF (ITerm iri)           = pure (unIRI iri)
  fromRDF (LTerm (PlainL x))    = pure x
  fromRDF (LTerm (PlainLL x _)) = pure x
  fromRDF (LTerm (TypedL x _))  = pure x
  fromRDF _ = Nothing

instance FromRDF String where
  fromRDF x = T.unpack <$> fromRDF x

instance FromRDF (IRI Abs) where
  fromRDF (ITerm x) = pure x
  fromRDF _ = Nothing
