module LinkedData.Utils
  ( isBlankNode
  , isQueryVar
  , subjectOf
  , predicateOf
  , objectOf
  )

where

import LinkedData.Types (Triple(..), Term(..))


-- | Is term a blank node?
isBlankNode :: Term -> Bool
isBlankNode (BNode _) = True
isBlankNode (BNodeGen _) = True
isBlankNode _ = False

-- | Is term a query variable?
isQueryVar :: Term -> Bool
isQueryVar (Var _) = True
isQueryVar _       = False

-- | Get subject of triple.
subjectOf :: Triple -> Term
subjectOf (Triple s _ _) = s

-- | Get predicate of triple.
predicateOf :: Triple -> Term
predicateOf (Triple _ p _) = p

-- | Get object of triple.
objectOf :: Triple -> Term
objectOf (Triple _ _ o) = o
