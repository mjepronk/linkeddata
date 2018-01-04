{-# LANGUAGE OverloadedStrings #-}

module Data.LinkedData.PropertyTests where

import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import           Data.LinkedData.Types
import qualified Data.LinkedData.Types as LD
import qualified Data.LinkedData.IRI as LD
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Text.URI as URI
import           Test.QuickCheck (Arbitrary, Gen, oneof, arbitrary, sized, choose)


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
        base <- arbitrary 
        pure (LD.emptyGraph { triples = ts, base = base })

instance Arbitrary IRI where
    arbitrary = IRI . URI.render <$> arbitrary


arbitraryTriples :: Gen Triples
arbitraryTriples = do
    n <- sized (\_ -> choose (0, 100)) :: Gen Int
    sequence [arbitrary | _ <- [1..n]]

arbitrarySubject, arbitraryPredicate, arbitraryObject :: Gen Term
arbitrarySubject = oneof [iterms, bnodes]
arbitraryPredicate = iterms
arbitraryObject = oneof [iterms, bnodes, lterms]

iterms :: Gen Term
iterms = ITerm . URI.render <$> arbitrary

lterms :: Gen Term
lterms = oneof $ pure . LTerm <$> literals

bnodes :: Gen Term
bnodes = oneof $ pure <$> bnodes' <> bnodegens

bnodes' :: [Term]
bnodes' = BNode <$> (\i -> T.pack "_genid" `T.append` T.pack (show i)) <$> [1..5]

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

datatypes :: [IRI]
datatypes = LD.unsafeAppendIRI xsdNS . IRI <$> ["string", "int", "token"]
