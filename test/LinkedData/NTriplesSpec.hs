module LinkedData.NTriplesSpec where


import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (mapMaybe, catMaybes, fromJust)
import           Data.List (sort, isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Monoid ((<>))
import           LinkedData.Types
import           LinkedData ((.:.), IRI(..))
import qualified LinkedData as LD
import           System.FilePath (takeFileName)
import           System.Directory (getCurrentDirectory)
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (property, withMaxSuccess)

import LinkedData.CommonSpec (shouldBeIsomorphicTo)
import LinkedData.Serialisation

spec :: Spec
spec =
  describe "N-Triples" $
    it "serialises and parses a random graph" $
      withMaxSuccess 20 $ property (\g -> do
        x <- runResourceT $ toGraph $ parseNTriples (serialiseNTriples (fromGraph g))
        case x of
          Right g' -> g' `shouldBeIsomorphicTo` g
          Left err -> expectationFailure ("Parser error: " <> show err))
