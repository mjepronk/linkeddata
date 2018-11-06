{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Hspec

import qualified LinkedData.FormatSpec
import qualified LinkedData.FromRDFSpec
import qualified LinkedData.IRISpec
import qualified LinkedData.NTriplesSpec
import qualified LinkedData.TurtleSpec
import qualified LinkedData.QuerySpec


main :: IO ()
main = hspec $ do

  describe "Hashable instances" $ it "todo" $ pending
  describe "isIsomorphicTo" $ it "todo" $ pending

  LinkedData.FormatSpec.spec
  LinkedData.FromRDFSpec.spec
  LinkedData.IRISpec.spec
  LinkedData.NTriplesSpec.spec
  LinkedData.TurtleSpec.spec
  LinkedData.QuerySpec.spec
