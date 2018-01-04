{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Control.Monad (unless)
import qualified Data.Text as T
import           Data.Monoid ((<>))
import Test.Hspec
import Path (Path, Rel, File, parseRelFile, toFilePath)
-- import Test.Hspec.Megaparsec

import           Data.LinkedData.Types
import qualified Data.LinkedData.Graphs as LD
import qualified Data.LinkedData.IRI as LD
import           Data.LinkedData.PropertyTests
import qualified Data.LinkedData.Query as LD
import qualified Data.LinkedData.Serialisation.TurtleParser as LD
import qualified Data.LinkedData.Types as LD
import           Test.QuickCheck (Arbitrary, property)


-- data TurtleTestCase = TurtleTestCase
--   { name    :: T.Text
--   , comment :: T.Text
--   , action  :: T.Text --Path Rel File
--   , result  :: T.Text --Path Rel File
--   }

processManifest :: Path Rel File -> IO Triples--[TurtleTestCase]
processManifest path = do
    let mfNS = IRI "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#"
        --qtNS = IRI "http://www.w3.org/2001/sw/DataAccess/tests/test-query#"
        rdftNS = IRI "http://www.w3.org/ns/rdftest#"
    g <- LD.parseTurtleFile (toFilePath path) -- TODO parseTurtleFile should take Path
    case g of
      Right (g@Graph { triples }) ->
        pure triples
        -- pure $ LDselect g'
        --   [ Var "name", Var "comment", Var "action", Var "result" ]
        --   [ Triple (Var "uri") (LDiterm rdfNS (IRI "type")) (LDiterm rdftNS (IRI "TestTurtleEval"))
        --   , Triple (Var "uri") (LDiterm mfNS (IRI "name")) (Var "name")
        --   , Triple (Var "uri") (LDiterm rdfsNS (IRI "comment")) (Var "comment")
        --   , Triple (Var "uri") (LDiterm mfNS (IRI "action")) (Var "action")
        --   , Triple (Var "uri") (LDiterm mfNS (IRI "result")) (Var "result") -- optional
        --   ]
      Left e -> fail $ show e
 
main :: IO ()
main = hspec $ do
  describe "TurtleParser" $ do
    it "parses test manifest" $ do
      manifestP <- parseRelFile "test/w3c/turtle/manifest.ttl"
      length <$> processManifest manifestP `shouldReturn` 2173
  describe "Query" $ do
    it "queries all triples from an arbitrary graph" $ do
      property $ (\g ->
        let g' = emptyGraph { triples = LD.query g (Var "a") (Var "b") (Var "c") }
        in  g `shouldBeIsomorphicTo` g')


shouldBeIsomorphicTo :: Graph -> Graph -> Expectation
shouldBeIsomorphicTo a b = unless (a `LD.isIsomorphic` b) e
  where e = expectationFailure (
          "The following two graphs are not isomorphic '"
          <> show a <> "' and '" <> show b <> "'.")
