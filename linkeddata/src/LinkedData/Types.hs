-- | RDF types
--
-- See:
-- - https://www.w3.org/TR/rdf11-concepts/
--
-- TODO:
-- - remove Var data constructor from Term
-- - add class instances

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE QuasiQuotes       #-}

module LinkedData.Types
  ( Graph(..)
  , emptyGraph
  , GraphMeta(..)
  , emptyGraphMeta

  -- * Triple and term.
  , Triple(..)
  , Triples
  , Term(..)
  , LValue(..)

  -- * Smart constructors for triples and terms.
  , triple
  , plainL
  , plainLL
  , typedL

  -- * Common RDF namespaces.
  , Namespaces
  , owlNS, rdfNS, rdfsNS, rdfaNS, xmlNS, xsdNS
  , foafNS, skosNS, dcNS, dctNS
  , commonNamespaces

  -- * Serialisation and parser errors.
  , SerialisationError(..)
  )
where

import           Data.ByteString (ByteString)
import           Data.Monoid ((<>))
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Binary (Binary)
import           GHC.Generics (Generic)
import           Test.QuickCheck (Arbitrary, Gen, oneof, arbitrary, sized, choose)

import LinkedData.IRI (IRI, Abs, Prefix(..), Namespaces, (.:.))
import LinkedData.QQ (absiri, reliri)


-- | RDF Graph
data Graph = Graph {
    triples    :: Triples
  , meta       :: GraphMeta
  }
  deriving Show

emptyGraph :: Graph
emptyGraph = Graph {
    triples    = []
  , meta       = emptyGraphMeta
  }

data GraphMeta = GraphMeta {
    base       :: Maybe (IRI Abs)
  , namespaces :: M.Map Prefix (IRI Abs)
  }
  deriving Show

emptyGraphMeta :: GraphMeta
emptyGraphMeta = GraphMeta {
    base       = Nothing
  , namespaces = M.empty
  }


-- | RDF Triple
-- TODO: should we add bang patterns on the terms?
-- TODO: should we add a quad type?
data Triple = Triple Term Term Term
  deriving (Eq, Ord, Show)

type Triples = [Triple]


-- | Smart constructor for a RDF triple
triple :: Term  -- ^ Subject
       -> Term  -- ^ Predicate
       -> Term  -- ^ Object
       -> Either T.Text Triple
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
data Term = ITerm (IRI Abs)  -- ^ IRI term
          | LTerm LValue     -- ^ Literal term
          | BNode T.Text     -- ^ Blank node with label
          | BNodeGen Int     -- ^ Generated blank node with number
          | Var T.Text       -- ^ Query variable
  deriving (Eq, Ord, Generic)

instance Binary Term

instance Show Term where
  show (ITerm i)    = show i
  show (LTerm v)    = show v
  show (BNode s)    = "_:" <> T.unpack s
  show (BNodeGen i) = "_:a" <> show i
  show (Var s)      = "?" <> T.unpack s


-- | Literal value
data LValue = PlainL T.Text            -- ^ Plain literal
            | PlainLL T.Text T.Text    -- ^ Plain literal with language identifier
            | TypedL T.Text (IRI Abs)  -- ^ Plain literal with type
  deriving (Eq, Ord, Generic)

instance Binary LValue

instance Show LValue where
  show (PlainL s)    = "\"" <> T.unpack s <> "\""
  show (PlainLL s l) = "\"" <> T.unpack s <> "\"@" <> T.unpack l
  show (TypedL s t)  = "\"" <> T.unpack s <> "\"^^" <> show t

plainL :: T.Text -> Term
plainL s = LTerm (PlainL s)

plainLL :: T.Text -> T.Text -> Term
plainLL s l = LTerm (PlainLL s l)

typedL :: T.Text -> IRI Abs -> Term
typedL s t = LTerm (TypedL s t)


-- | Common RDF namespaces.

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


-- | Serialisation and parser errors.

data SerialisationError = SParsingError T.Text [String] String
                        | SDecodingError ByteString
                        -- | SSerialisingError Triple

instance Show SerialisationError where
  show (SParsingError t _ err) = "Parsing error: " <> err <> " in " <> T.unpack t <> "."
  show (SDecodingError _) = "Decoding error."
  -- show (SSerialisingError t) = "Serialisation error: " <> show t




instance Arbitrary Triple where
    arbitrary = Triple
                <$> arbitrarySubject
                <*> arbitraryPredicate
                <*> arbitraryObject

instance Arbitrary Term where
    arbitrary = iterms

instance Arbitrary LValue where
    arbitrary = oneof $ pure <$> literals

instance Arbitrary Graph where
    arbitrary = do
        ts <- arbitraryTriples
        pure (emptyGraph { triples = ts })


arbitraryTriples :: Gen Triples
arbitraryTriples = do
    n <- sized (\_ -> choose (0, 100)) :: Gen Int
    sequence [arbitrary | _ <- [1..n]]

arbitrarySubject, arbitraryPredicate, arbitraryObject :: Gen Term
arbitrarySubject = oneof [iterms, bnodes]
arbitraryPredicate = iterms
arbitraryObject = oneof [iterms, bnodes, lterms]

iterms :: Gen Term
iterms = ITerm <$> arbitrary

lterms :: Gen Term
lterms = oneof $ pure . LTerm <$> literals

bnodes :: Gen Term
bnodes = oneof $ pure <$> bnodes' <> bnodegens

bnodes' :: [Term]
bnodes' = BNode . (\i -> "arb" `T.append` T.pack (show i)) <$> ([1..5] :: [Int])

bnodegens :: [Term]
bnodegens = BNodeGen <$> [1..5]

literals :: [LValue]
literals = plainLiterals <> plainLLiterals <> typedLiterals

plainLiterals :: [LValue]
plainLiterals = [PlainL lit | lit <- litValues]

plainLLiterals :: [LValue]
plainLLiterals = [PlainLL lit lang | lit <- litValues, lang <- languages]

typedLiterals :: [LValue]
typedLiterals = [TypedL lit dtype | lit <- litValues, dtype <- datatypes]

litValues :: [Text]
litValues = ["hello", "world", "peace", "earth", "", "haskell"]

languages :: [Text]
languages = ["fr", "en", "nl-nl"]

datatypes :: [IRI Abs]
datatypes = (xsdNS .:.) <$> [
  [reliri|string|],
  [reliri|int|],
  [reliri|token|]]
