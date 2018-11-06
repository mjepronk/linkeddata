{-# LANGUAGE OverloadedStrings #-}

module LinkedData.FormatSpec
  ( spec )
where

import           GHC.Real (infinity, notANumber)
import           Data.Either (isLeft, fromRight)
import           Data.Ratio ((%))
import qualified Data.Text as T
import           Data.Time.Calendar (fromGregorian)
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)

import LinkedData (parseRational, formatRational, parseInteger, formatInteger,
           parseBool, formatBool, parseRealFloat, formatRealFloat,
           parseDate, formatDate)

spec :: Spec
spec = describe "Format" $ do
  context "parseRational" $ do
    it "parses a decimal" $ parseRational "3.14159" `shouldBe` Right (314159 % 100000)
    it "parses a negative decimal" $ parseRational "-3.14159" `shouldBe` Right ((-314159) % 100000)
    it "parses a positive decimal" $ parseRational "+3.14159" `shouldBe` Right (314159 % 100000)
    it "rejects scientific notation" $ parseRational "1e1000000000" `shouldSatisfy` isLeft
    it "rejects invalid" $ parseRational "tweeenveertig" `shouldSatisfy` isLeft
  context "formatRational" $ do
    it "formats a decimal" $ formatRational (314159 % 100000) `shouldBe` "3.14159"
    it "formats a negative decimal" $ formatRational ((-314159) % 100000) `shouldBe` "-3.14159"
    it "formats an infinite rational" $ formatRational (1 % 3) `shouldSatisfy` ("0.3333" `T.isPrefixOf`)
  context "parseInteger" $ do
    it "parses an integer" $ parseInteger "3814986502092304" `shouldBe` Right 3814986502092304
    it "parses a positive integer" $ parseInteger "+3814986502092304" `shouldBe` Right 3814986502092304
    it "parses a negative integer" $ parseInteger "-26315271553053477373" `shouldBe` Right (-26315271553053477373)
  context "formatInteger" $ do
    it "formats an integer" $ formatInteger 3814986502092304 `shouldBe` "3814986502092304"
    it "formats a negative integer" $ formatInteger (-26315271553053477373) `shouldBe` "-26315271553053477373"
  context "parseBool" $ do
    it "parses true" $ parseBool "true" `shouldBe` Right True
    it "parses false" $ parseBool "0" `shouldBe` Right False
    it "parses 1" $ parseBool "1" `shouldBe` Right True
    it "parses 0" $ parseBool "0" `shouldBe` Right False
    it "rejects invalid input" $ parseBool "waar" `shouldSatisfy` isLeft
  context "formatBool" $ do
    it "formats true" $ formatBool True `shouldBe` "true"
    it "formats false" $ formatBool False `shouldBe` "false"
  context "parseRealFloat" $ do
    it "parses a float" $ parseRealFloat "3.14159" `shouldBe` Right (3.14159 :: Double)
    it "parses a negative float" $ parseRealFloat "-3.14159" `shouldBe` Right (-3.14159 :: Double)
    it "parses a positive float" $ parseRealFloat "+3.14159" `shouldBe` Right (3.14159 :: Double)
    it "parses scientific notation" $ parseRealFloat "1e100" `shouldBe` Right (1.0e100 :: Double)
    it "parses NaN" $ (parseRealFloat "NaN" :: Either String Double) `shouldSatisfy` isNaN . fromRight 0
    it "parses INF" $ parseRealFloat "INF" `shouldBe` Right inf
    it "parses -INF" $ parseRealFloat "-INF" `shouldBe` Right neginf
  context "formatRealFloat" $ do
    it "formats a float" $ formatRealFloat (3.14159 :: Double) `shouldBe` "3.14159"
    it "formats a negative float" $ formatRealFloat (-3.14159 :: Double) `shouldBe` "-3.14159"
    it "formats a positive float" $ formatRealFloat (3.14159 :: Double) `shouldBe` "3.14159"
    it "formats scientific notation" $ formatRealFloat (1.0e100 :: Double) `shouldBe` "1.0e100"
    it "formats NaN" $ formatRealFloat (realToFrac notANumber :: Double) `shouldBe` "NaN"
    it "formats INF" $ formatRealFloat inf `shouldBe` "INF"
    it "formats -INF" $ formatRealFloat neginf `shouldBe` "-INF"
  context "parseDate" $ do
    it "parses a date" $ parseDate "1983-12-08" `shouldBe` Right (fromGregorian 1983 12 8)
    it "parses a date with a negative year" $ parseDate "-1983-12-08" `shouldBe` Right (fromGregorian (-1983) 12 8)
    it "rejects an invalid date" $ parseDate "2018-02-29" `shouldSatisfy` isLeft
    it "rejects a date with year less than 4 digits" $ parseDate "83-12-08" `shouldSatisfy` isLeft
    it "rejects a date with year more than 4 digits and leading zeroes" $
        parseDate "01983-12-08" `shouldSatisfy` isLeft
    it "rejects a date with year 0000" $
        parseDate "0000-12-08" `shouldSatisfy` isLeft
  context "formatDate" $ do
    it "formats a date" $ formatDate (fromGregorian 1983 12 8) `shouldBe` "1983-12-08"
  it "parseTime" $ pending
  it "formatTime" $ pending
  it "parseDateTime" $ pending
  it "formatDateTime" $ pending

inf :: Double
inf = realToFrac infinity

neginf :: Double
neginf = negate (realToFrac infinity)
