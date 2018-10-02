{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Hspec

import qualified LinkedData.IRISpec
import qualified LinkedData.NTriplesSpec
import qualified LinkedData.TurtleSpec
import qualified LinkedData.QuerySpec


main :: IO ()
main = hspec $ do

  describe "Hash functions" $ it "todo" $ pending
  describe "isIsomorphicTo" $ it "todo" $ pending

  LinkedData.IRISpec.spec
  LinkedData.NTriplesSpec.spec
  LinkedData.TurtleSpec.spec
  LinkedData.QuerySpec.spec

  describe "SPARQL" $ it "todo" $ pending
