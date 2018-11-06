{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module LinkedData.QuerySpec where

import           Data.Monoid ((<>))
import           LinkedData (Graph, Triple(..), TriplePattern(..), Term(..),
                     Var(..), IRI, Abs, (.:.), plainL, foafNS, rdfNS, rdfType,
                     query, select)
import           LinkedData.Serialisation (parseTurtleFile, toGraphWithMeta, runResourceT)
import           LinkedData.QQ (absiri, reliri)
import           System.Directory (getCurrentDirectory)
import           System.FilePath ((</>))
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess)

spec :: Spec
spec = do
  describe "Query" $ modifyMaxSuccess (const 20) $ do
    it "queries a triple from a sample graph" $ do
      g <- parseH2G2File
      -- Query the name of the person that is identified by the IRI
      -- h2g2:Arthur+Dent
      let arthur = ITerm $ h2g2NS .:. [reliri|Arthur+Dent|]
          name   = ITerm $ foafNS .:. [reliri|name|]
          result = query g (TriplePattern (Right arthur) (Right name) (Left (Var "name")))
      result `shouldBe` [Triple arthur name (plainL "Arthur Dent")]
    it "queries multiple triples from a sample graph" $ do
      g <- parseH2G2File
      -- Query all the people in the graph
      let person = ITerm $ foafNS .:. [reliri|Person|]
          a      = ITerm $ rdfType
          result = query g (TriplePattern (Left (Var "person")) (Right a) (Right person))
      result `shouldBe` [
          Triple (ITerm $ h2g2NS .:. [reliri|Arthur+Dent|])     a person
        , Triple (ITerm $ h2g2NS .:. [reliri|Ford+Prefect|])    a person
        , Triple (ITerm $ h2g2NS .:. [reliri|Tricia+McMillan|]) a person
        ]
  describe "Select" $
    it "queries a sample graph" $ do
      g <- parseH2G2File
      -- Who does Arthur Dent know?
      let arthur  = Var "arthur"
          who     = Var "who"
          name    = Var "name"
          clauses = [
              TriplePattern (Left arthur) (Right . ITerm $ rdfNS .:. [reliri|type|]) (Right . ITerm $ foafNS .:. [reliri|Person|])
            , TriplePattern (Left arthur) (Right . ITerm $ foafNS .:. [reliri|name|]) (Right (plainL "Arthur Dent"))
            , TriplePattern (Left arthur) (Right . ITerm $ foafNS .:. [reliri|knows|]) (Left who)
            , TriplePattern (Left who) (Right . ITerm $ foafNS .:. [reliri|name|]) (Left name)
            ]
          result  = select g [name] clauses
      result `shouldBe` [[plainL "Ford Prefect"], [plainL "Tricia McMillan"]]


-- Hitchhikers Guide sample graph
h2g2NS :: IRI Abs
h2g2NS = [absiri|http://example.com/h2g2/|]

parseH2G2File :: IO Graph
parseH2G2File = do
  cwd <- getCurrentDirectory
  g <- runResourceT $ toGraphWithMeta (parseTurtleFile Nothing (cwd </> "test/hhgttg.ttl"))
  case g of
    Left e -> fail $ "Error while parsing hhgttg.ttl: " <> show e
    Right g' -> pure g'
