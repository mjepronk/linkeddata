{-# LANGUAGE OverloadedStrings #-}

module LinkedData
  ( module LinkedData.Format
  , module LinkedData.FromRDF
  , module LinkedData.Graphs
  , module LinkedData.IRI
  , module LinkedData.Namespaces
  , module LinkedData.Query
  , module LinkedData.ToRDF
  , module LinkedData.Types
  , module LinkedData.Utils

  , triple
  )
where

import LinkedData.Format
import LinkedData.FromRDF
import LinkedData.Graphs
import LinkedData.IRI
import LinkedData.Namespaces
import LinkedData.Query
import LinkedData.ToRDF
import LinkedData.Types
import LinkedData.Utils

import qualified Data.Text as T

-- | Smart constructor for a RDF triple
triple :: (ToRDF s, ToRDF p, ToRDF o)
       => s  -- ^ Subject
       -> p  -- ^ Predicate
       -> o  -- ^ Object
       -> Either T.Text Triple
triple s p o =
  Triple <$> (validateSubject s'   *> pure s')
         <*> (validatePredicate p' *> pure p')
         <*> pure o'
  where
    s' = toRDF s
    p' = toRDF p
    o' = toRDF o

    validateSubject x@(ITerm _)    = pure x
    validateSubject x@(BNode _)    = pure x
    validateSubject x@(BNodeGen _) = pure x
    validateSubject _              = Left "Subject must be an IRI or a blank node."

    validatePredicate x@(ITerm _) = pure x
    validatePredicate _           = Left "Predicate must be an IRI."
