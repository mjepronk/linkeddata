{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad (unless, forM_)
import           Data.Maybe (mapMaybe)
import           Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Monoid ((<>))
import           Data.LinkedData.Types
import qualified Data.LinkedData as LD
import           Data.LinkedData ((#))
import           Data.LinkedData.PropertyTests ()
import           Path (Path, Rel, File, parseRelFile, toFilePath)
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (property, withMaxSuccess)


data TurtleTestCase = TurtleTestCase
  { name    :: Term  -- Text
  , comment :: Term  -- Text
  , action  :: Term  -- Text --Path Rel File
  , result  :: Term  -- Text --Path Rel File
  }

processManifest :: Path Rel File -> IO [TurtleTestCase]
processManifest path = do
    let mfNS = IRI "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#"
        --qtNS = IRI "http://www.w3.org/2001/sw/DataAccess/tests/test-query#"
        rdftNS = IRI "http://www.w3.org/ns/rdftest#"
    g <- LD.parseTurtleFile path
    case g of
      Right g' ->
        let test = Var "test"
            name = Var "name"
            comment = Var "comment"
            action = Var "action"
            result = Var "result"
            res = LD.select g'
              [ name, comment, action, result ]
              [ Triple test (ITerm $ rdfNS # "type") (ITerm $ rdftNS # "TestTurtleEval")
              , Triple test (ITerm $ mfNS # "name") name
              , Triple test (ITerm $ rdfsNS # "comment") comment
              , Triple test (ITerm $ mfNS # "action") action
              , Triple test (ITerm $ mfNS # "result") result -- optional
              ]
        in  pure (mapMaybe resultToTC res)
      Left e -> fail $ show e
  where
    resultToTC :: [Term] -> Maybe TurtleTestCase
    resultToTC (name : comment : action : result : []) = pure (TurtleTestCase { .. })
    resultToTC _ = Nothing


main :: IO ()
main = hspec $ do
  describe "IRI" $
    it "resolves the fragment component according to RDF spec" $
      (IRI "fragment") `LD.relativeTo` (IRI "http://example.com/test#")
          `shouldBe` Just (IRI "http://example.com/test#fragment")
  describe "Hash functions" $ it "todo" $ pending
  describe "isIsomorphicTo" $ it "todo" $ pending
  describe "N-Triples" $
    it "serialises and parses a random graph" $
      withMaxSuccess 20 $ property (\g ->
        case LD.parseNTriples Nothing (LD.serialiseNTriples g) of
          Left e -> expectationFailure ("Parser error: " <> show e)
          Right g' -> g' `shouldBeIsomorphicTo` g)
  describe "Turtle" $ do
    it "serialises and parses a random graph" $
      withMaxSuccess 20 $ property (\g ->
        case LD.parseTurtle Nothing (LD.serialiseTurtle g) of
          Left e -> expectationFailure ("Parser error: " <> show e)
          Right g' -> g' `shouldBeIsomorphicTo` g)
    it "parses, serialises and parses the test manifest again" $ do
      manifestP <- parseRelFile "test/w3c/turtle/manifest.ttl"
      input <- LD.parseTurtleFile manifestP
      case input of
        Left e -> expectationFailure ("Error while parsing test manifest: " <> show e)
        Right input' ->
          let serialised = LD.serialiseTurtle input'
          in  case LD.parseTurtle Nothing serialised of
                Left e -> expectationFailure (
                  "Error while parsing serialised file: " <> show e <> "\n\n" <> TL.unpack serialised)
                Right output -> output `shouldBeIsomorphicTo` input'
    context "W3C Turtle test suite" $ do
      manifestP <- runIO $ parseRelFile "test/w3c/turtle/manifest.ttl"
      manifest <- runIO $ processManifest manifestP
      forM_ manifest $ (\t ->
        it ("it " <> show (name t)) $ True `shouldBe` True)
  describe "Query" $ modifyMaxSuccess (const 20) $ do
    it "queries all triples from an arbitrary graph" $
      property $ (\g ->
        let g' = LD.emptyGraph { triples = LD.query g (Var "a") (Var "b") (Var "c") }
        in  g' `shouldBeIsomorphicTo` g)
    it "queries a triple from a sample graph" $ do
      f <- parseRelFile "test/hhgttg.ttl"
      g <- LD.parseTurtleFile f
      case g of
        Left e -> expectationFailure $ "Error while parsing hhgttg.ttl: " <> show e
        Right g' ->
          let h2g2NS = IRI "http://example.com/h2g2/"
              t = LD.query g' (ITerm $ h2g2NS # "Arthur+Dent") (ITerm $ foafNS # "name") (Var "name")
              exp = Triple (ITerm $ h2g2NS # "Arthur+Dent") (ITerm $ foafNS # "name") (plainL "Arthur Dent")
          in  t `shouldBe` [exp]
  describe "Select" $
    it "queries a sample graph" $ do
      f <- parseRelFile "test/hhgttg.ttl"
      g <- LD.parseTurtleFile f
      case g of
        Left e -> expectationFailure $ "Error while parsing hhgttg.ttl: " <> show e
        Right g' ->
          -- Who does Arthur Dent know?
          let arthur = Var "arthur"
              who = Var "who"
              name = Var "name"
              clauses = [
                Triple arthur (ITerm $ rdfNS # "type") (ITerm $ foafNS # "Person"),
                Triple arthur (ITerm $ foafNS # "name") (plainL "Arthur Dent"),
                Triple arthur (ITerm $ foafNS # "knows") who,
                Triple who (ITerm $ foafNS # "name") name]
              r = LD.select g' [name] clauses
          in  r `shouldBe` [[plainL "Ford Prefect"], [plainL "Tricia McMillan"]]
  describe "SPARQL" $ it "todo" $ pending


shouldBeIsomorphicTo :: Graph -> Graph -> Expectation
shouldBeIsomorphicTo result expected =
    case (result `LD.isIsomorphicWithDiff` expected) of
      Left (rts, ets) -> expectationFailure
          (T.unpack $ "The two graphs are not isomorphic, below are the triples in result but not in expected:\n\n"
          <> (TL.toStrict $ LD.serialiseNTriples (emptyGraph{ triples=rts }))
          <> "\nand triples in expected but not in result:\n\n"
          <> (TL.toStrict $ LD.serialiseNTriples (emptyGraph{ triples=ets }))
          <> "\n")
      Right () -> pure ()
