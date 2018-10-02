{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module LinkedData.QuerySpec where

import           Data.Monoid ((<>))
import           LinkedData.Types
import           LinkedData (IRI, Abs, (.:.))
import qualified LinkedData as LD
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
          result = LD.query g arthur name (Var "name")
      result `shouldBe` [Triple arthur name (plainL "Arthur Dent")]
    it "queries multiple triples from a sample graph" $ do
      g <- parseH2G2File
      -- Query all the people in the graph
      let person = ITerm $ foafNS .:. [reliri|Person|]
          a      = ITerm $ rdfNS .:. [reliri|type|]
          result = LD.query g (Var "person") a person
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
              Triple arthur (ITerm $ rdfNS .:. [reliri|type|]) (ITerm $ foafNS .:. [reliri|Person|])
            , Triple arthur (ITerm $ foafNS .:. [reliri|name|]) (plainL "Arthur Dent")
            , Triple arthur (ITerm $ foafNS .:. [reliri|knows|]) who
            , Triple who (ITerm $ foafNS .:. [reliri|name|]) name
            ]
          result  = LD.select g [name] clauses
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
