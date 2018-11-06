{-# LANGUAGE OverloadedStrings #-}

module LinkedData.Common
  ( shouldBeIsomorphicTo )
where

import Control.Monad (join)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Streaming.Prelude as S
import           Test.Hspec

import           LinkedData (Graph, Triples)
import qualified LinkedData as LD
import           LinkedData.Serialisation


shouldBeIsomorphicTo :: Graph -> Graph -> Expectation
shouldBeIsomorphicTo result expected =
    case result `LD.isIsomorphicWithDiff` expected of
      Left (rts, ets) ->
        expectationFailure
        (T.unpack $ "The two graphs are not isomorphic, below are the triples in result but not in expected:\n\n"
        <> triplesToText rts
        <> "\nand triples in expected but not in result:\n\n"
        <> triplesToText ets
        <> "\n")
      Right () -> pure ()

  where
    triplesToText :: Triples -> T.Text
    triplesToText = join (S.mconcat_ . serialiseNTriples . S.each)
