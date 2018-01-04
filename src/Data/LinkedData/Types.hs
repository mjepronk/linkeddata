-- | RDF types
-- 
-- See:
-- - https://www.w3.org/TR/rdf11-concepts/
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.LinkedData.Types where

import           Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Binary (Binary)
import           GHC.Generics (Generic)


-- | RDF Graph
data Graph = Graph {
    triples    :: Triples
  , base       :: Maybe IRI
  , namespaces :: M.Map Prefix IRI
  }
  deriving Show

emptyGraph :: Graph
emptyGraph = Graph {
    triples    = []
  , base       = Nothing
  , namespaces = M.empty
  }


-- | RDF Triple
data Triple = Triple Term Term Term
  deriving (Eq, Ord, Show)

type Triples = [Triple]


-- | Smart constructor for a RDF triple
triple :: Term -> Term -> Term -> Either T.Text Triple
triple s p o =
  Triple <$> (validateSubject s   *> pure s)
         <*> (validatePredicate p *> pure p)
         <*> (validateObject o    *> pure o)
  where
    validateSubject s'@(ITerm _)    = pure s'
    validateSubject s'@(BNode _)    = pure s'
    validateSubject s'@(BNodeGen _) = pure s'
    validateSubject _               = Left "Subject must be an IRI or a blank node."

    validatePredicate p'@(ITerm _) = pure p'
    validatePredicate _            = Left "Predicate must be an IRI."

    validateObject (Var _) = Left "Object may not be a query variable."
    validateObject o'      = pure o'


-- | RDF Term
data Term = ITerm T.Text  -- ^ IRI term
          | LTerm LValue  -- ^ Literal term
          | BNode T.Text  -- ^ Blank node with label
          | BNodeGen Int  -- ^ Generated blank node with number
          | Var T.Text    -- ^ Query variable
  deriving (Eq, Ord, Generic)

instance Binary Term

instance Show Term where
  show (ITerm i)    = "<" <> (T.unpack i) <> ">"
  show (LTerm v)    = show v
  show (BNode s)    = "_:" <> (T.unpack s)
  show (BNodeGen i) = "_:a" <> show i
  show (Var s)      = "?" <> (T.unpack s)


-- | Literal value
data LValue = PlainL T.Text          -- ^ Plain literal
            | PlainLL T.Text T.Text  -- ^ Plain literal with language identifier
            | TypedL T.Text IRI      -- ^ Plain literal with type
  deriving (Eq, Ord, Generic)

instance Binary LValue

instance Show LValue where
  show (PlainL s)    = "\"" <> T.unpack s <> "\""
  show (PlainLL s l) = "\"" <> T.unpack s <> "\"@" <> T.unpack l
  show (TypedL s (IRI t)) = "\"" <> T.unpack s <> "\"^^<" <> T.unpack t <> ">"

plainL :: T.Text -> Term
plainL s = LTerm (PlainL s)

plainLL :: T.Text -> T.Text -> Term
plainLL s l = LTerm (PlainLL s l)

typedL :: T.Text -> IRI -> Term
typedL s t = LTerm (TypedL s t)


-- | Prefix (short identifier for an IRI)
newtype Prefix = Prefix T.Text
  deriving (Eq, Ord, Show)


-- | Internationalized Resource Identifier (similar to URI's)
newtype IRI = IRI T.Text
  deriving (Eq, Ord, Show, Generic)

instance Binary IRI


-- | Commonly used namespaces
owlNS, rdfNS, rdfsNS, rdfaNS, xhvNS, xmlNS, xsdNS :: IRI
owlNS  = IRI "http://www.w3.org/2002/07/owl#"
rdfNS  = IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
rdfsNS = IRI "http://www.w3.org/2000/01/rdf-schema#"
rdfaNS = IRI "http://www.w3.org/ns/rdfa#"
xhvNS  = IRI "http://www.w3.org/1999/xhtml/vocab#"
xmlNS  = IRI "http://www.w3.org/XML/1998/namespace"
xsdNS  = IRI "http://www.w3.org/2001/XMLSchema#"
