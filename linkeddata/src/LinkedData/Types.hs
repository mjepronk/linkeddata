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

  -- * Triple, Term and Var.
  , Triple(..)
  , Triples
  , TriplePattern(..)
  , Term(..)
  , LValue(..)
  , Var(..)
  , VarOrTerm

  -- * Smart constructors for triples and terms.
  , plainL
  , plainLL
  , typedL

  , Namespaces

  -- * Serialisation and parser errors.
  , SerialisationError(..)
  )
where

import           Data.ByteString (ByteString)
import           Data.Hashable (Hashable(..))
import           Data.Monoid ((<>))
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Binary (Binary)
import           GHC.Generics (Generic)
import           Test.QuickCheck (Arbitrary, Gen, oneof, arbitrary, sized, choose)

import           LinkedData.IRI (IRI, Abs, Prefix(..), Namespaces, (.:.))
import           LinkedData.Namespaces (xsdNS)
import           LinkedData.QQ (reliri)


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
-- TODO: should we create a generic Triple? i.e. Triple a = Triple a a a
data Triple = Triple Term Term Term
  deriving (Eq, Ord, Show)

type Triples = [Triple]

-- | Triples that may only contain a Var or Term.
data TriplePattern = TriplePattern VarOrTerm VarOrTerm VarOrTerm
  deriving (Eq, Ord, Show)

-- | RDF Term
data Term = ITerm (IRI Abs)  -- ^ IRI term
          | LTerm LValue     -- ^ Literal term
          | BNode T.Text     -- ^ Blank node with label
          | BNodeGen Int     -- ^ Generated blank node with number
  deriving (Eq, Ord, Generic)

instance Binary Term

instance Hashable Term

instance Show Term where
  show (ITerm i)    = show i
  show (LTerm v)    = show v
  show (BNode s)    = "_:" <> T.unpack s
  show (BNodeGen i) = "_:a" <> show i


-- | Literal value
-- TODO: comparison on language identifier should be case insensitive.
data LValue = PlainL T.Text            -- ^ Plain literal
            | PlainLL T.Text T.Text    -- ^ Plain literal with language identifier
            | TypedL T.Text (IRI Abs)  -- ^ Plain literal with type
  deriving (Eq, Ord, Generic)

instance Binary LValue

instance Hashable LValue

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


-- | Query variable
data Var = Var T.Text
  deriving (Eq, Ord, Generic)

instance Show Var where
  show (Var s) = "?" <> T.unpack s

-- | Either RDF Term (Right) or Query variable (Left)
type VarOrTerm = Either Var Term


-- Serialisation and parser errors

data SerialisationError = SParsingError T.Text [String] String
                        | SDecodingError ByteString
                        -- | SSerialisingError Triple

instance Show SerialisationError where
  show (SParsingError t _ err) = "Parsing error: " <> err <> " in " <> T.unpack t <> "."
  show (SDecodingError _) = "Decoding error."
  -- show (SSerialisingError t) = "Serialisation error: " <> show t


-- QuickCheck instances
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
