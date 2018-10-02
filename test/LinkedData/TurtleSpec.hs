{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module LinkedData.TurtleSpec where


import           Control.Monad (forM_)
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Identity (runIdentity)
import           Data.List (stripPrefix)
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Monoid ((<>))
import           System.FilePath (takeFileName, (</>))
import           System.Directory (getCurrentDirectory)
import           Test.Hspec
import           Test.QuickCheck (property, withMaxSuccess)

import           LinkedData (IRI, Graph(..), Triple(..), Term(..), (.:.), emptyGraphMeta, fromRDF, select, rdfNS, rdfsNS, mkAbsIRI)
import           LinkedData.QQ (absiri, reliri)
import           LinkedData.Serialisation (runResourceT, toGraph, fromGraph, toGraphWithMeta, parseNTriplesFile, parseTurtle, parseTurtleFile, serialiseTurtle)
import           LinkedData.CommonSpec (shouldBeIsomorphicTo)


data TurtleTestCase = TurtleTestCase
  { name    :: T.Text
  , comment :: T.Text
  , action  :: FilePath
  , result  :: FilePath
  }

parseTurtleTestFile :: FilePath -> IO (Either String Graph)
parseTurtleTestFile fp = do
    g <- runResourceT . toGraphWithMeta $ parseTurtleFile base fp
    case g of
      Left e -> pure $ Left (show e)
      Right g -> pure $ Right g
  where
    base = mkAbsIRI ("http://www.w3.org/2013/TurtleTests/" <> (T.pack . takeFileName $ fp))

getManifestPath :: IO FilePath
getManifestPath = do
    cwd <- getCurrentDirectory
    pure (cwd </> "test/w3c/turtle/manifest.ttl")

stripFileScheme :: FilePath -> FilePath
stripFileScheme (stripPrefix "file://" -> Just rest) = rest
stripFileScheme fp = fp

parseManifest :: FilePath -> IO [TurtleTestCase]
parseManifest path = do
    let mfNS = [absiri|http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#|]
        rdftNS = [absiri|http://www.w3.org/ns/rdftest#|]
    g <- runResourceT (toGraph (parseTurtleFile Nothing path))
    case g of
      Right g' ->
        -- TODO: For now we only filter the tests of type rdft:TestTurtleEval,
        -- but we should also parse the other tests.
        -- However, we do not have the possibility yet to query optional values...
        let test = Var "test"
            name = Var "name"
            comment = Var "comment"
            action = Var "action"
            result = Var "result"
            res = select g'
              [ name, comment, action, result ]
              [ Triple test (ITerm $ rdfNS  .:. [reliri|type|]) (ITerm $ rdftNS .:. [reliri|TestTurtleEval|])
              , Triple test (ITerm $ mfNS   .:. [reliri|name|]) name
              , Triple test (ITerm $ rdfsNS .:. [reliri|comment|]) comment
              , Triple test (ITerm $ mfNS   .:. [reliri|action|]) action
              , Triple test (ITerm $ mfNS   .:. [reliri|result|]) result -- optional
              ]
        in  pure (mapMaybe resultToTC res)
      Left e -> fail $ "Error while parsing manifest " <> show path <> ": " <> show e
  where
    resultToTC :: [Term] -> Maybe TurtleTestCase
    resultToTC [name, comment, action, result] =
      TurtleTestCase
          <$> fromRDF name
          <*> fromRDF comment
          <*> fromRDF action
          <*> fromRDF result
    resultToTC _ = Nothing


spec :: Spec
spec =
  describe "Turtle" $ do
    it "serialises and parses a random graph" $
      withMaxSuccess 20 $ property (\g ->
        case runIdentity . toGraph $ parseTurtle Nothing (serialiseTurtle emptyGraphMeta (fromGraph g)) of
          Left e -> expectationFailure ("Parser error: " <> show e)
          Right g' -> g' `shouldBeIsomorphicTo` g)
    it "parses, serialises and parses (again) the test manifest" $ do
      manifestP <- getManifestPath
      x <- runResourceT . toGraph $ parseTurtleFile Nothing manifestP
      case x of
        Left e -> expectationFailure ("Error while parsing test manifest: " <> show e)
        Right g ->
          case runIdentity . toGraph $ parseTurtle Nothing (serialiseTurtle (meta g) (fromGraph g)) of
            Left e -> expectationFailure ("Error while parsing serialised file: " <> show e)
            Right g' -> g' `shouldBeIsomorphicTo` g
    context "W3C Turtle test suite" $ do
      manifestP <- runIO getManifestPath
      manifest <- runIO (parseManifest manifestP)
      forM_ manifest (\TurtleTestCase { .. } ->
        it ("passes the test '" <> T.unpack name <> "'") $ do
          actionR  <- parseTurtleTestFile (stripFileScheme action)
          expected <- runResourceT . toGraph $ parseNTriplesFile (stripFileScheme result)
          case actionR of
            Left e -> fail $ "Failed parsing action: " <> e
            Right actionR' ->
              case expected of
                Left e -> fail $ "Failed parsing result: " <> show e
                Right expected' ->
                  actionR' `shouldBeIsomorphicTo` expected')
