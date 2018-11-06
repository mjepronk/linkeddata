module LinkedData.Utils
  ( isIRI
  , isBlank
  , isLiteral
  , subjectOf
  , predicateOf
  , objectOf
  )
where

import LinkedData.Types (Triple(..), Term(..))

-- | Is the term an IRI?
isIRI :: Term -> Bool
isIRI (ITerm _) = True
isIRI _         = False

-- | Is the term a blank node?
isBlank :: Term -> Bool
isBlank (BNode _)    = True
isBlank (BNodeGen _) = True
isBlank _            = False

-- | Is the term a literal?
isLiteral :: Term -> Bool
isLiteral (LTerm _) = True
isLiteral _         = False

-- | Get subject of triple.
subjectOf :: Triple -> Term
subjectOf (Triple s _ _) = s

-- | Get predicate of triple.
predicateOf :: Triple -> Term
predicateOf (Triple _ p _) = p

-- | Get object of triple.
objectOf :: Triple -> Term
objectOf (Triple _ _ o) = o
