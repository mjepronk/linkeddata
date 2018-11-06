{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module LinkedData.FromRDFSpec
  ( spec )
where

import Debug.Trace

import           Control.Monad (forM_)
import           Data.Maybe (isJust, fromJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Time.Calendar (Day)
import           Data.Time.LocalTime (LocalTime, ZonedTime, zonedTimeToUTC)
import           Numeric.Natural (Natural)
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck.Instances.Text
import           Test.QuickCheck.Instances.Time
import           Test.QuickCheck.Instances.Natural

import           LinkedData (FromRDF(..), ToRDF(..), Term, IRI, Abs, typedL,
                     xsdFloat, xsdDouble, xsdDecimal, xsdInt, xsdInteger,
                     xsdNonPositiveInteger)


spec :: Spec
spec = describe "FromRDF" $ do
    context "FromRDF is consistent with ToRDF" $ do
        prop "is consistent for Bool"      (isConsistent @Bool)
        prop "is consistent for Int"       (isConsistent @Int)
        prop "is consistent for Integer"   (isConsistent @Integer)
        prop "is consistent for Integer"   (isConsistent @Natural)
        prop "is consistent for Float"     (isConsistent @Float)
        prop "is consistent for Double"    (isConsistent @Double)
        prop "is consistent for Rational"  (isConsistentRat)
        prop "is consistent for String"    (isConsistent @String)
        prop "is consistent for Text"      (isConsistent @T.Text)
        prop "is consistent for Lazy Text" (isConsistent @TL.Text)
        prop "is consistent for Day"       (isConsistent @Day)
        prop "is consistent for LocalTime" (isConsistent @LocalTime)
        prop "is consistent for ZonedTime" (isConsistentZT)
        -- prop "is consistent for IRI Abs"   (isConsistent @(IRI Abs))
        -- prop "is consistent for Term"      (isConsistent @Term)
    context "Parses numeric types" $ do
        testRoundTrip   @Int      testInts
        testRoundTrip   @Integer  testInts
        testRoundTrip   @Rational testDecimals
        testRoundTripRF @Float    testFloats
        testRoundTripRF @Double   testFloats


isConsistent :: (FromRDF a, ToRDF a, Eq a) => a -> Bool
isConsistent x = fromRDF (toRDF x) == Just x

isConsistentRat :: Rational -> Bool
isConsistentRat x =
    case fromRDF (toRDF x) of
      Just x' -> realToFrac(abs(x - x')) < 1e-100
      Nothing -> False

isConsistentZT :: ZonedTime -> Bool
isConsistentZT x = (zonedTimeToUTC <$> fromRDF (toRDF x)) == Just (zonedTimeToUTC x)

testRoundTrip :: forall a . (FromRDF a, ToRDF a, Eq a, Show a) => [Term] -> SpecWith ()
testRoundTrip xs = forM_ xs (\x ->
  it ("parses, renders and parses again " <> show x) $ do
    let r  = fromJust $ fromRDF @a x
    let r' = fromJust $ fromRDF @a (toRDF r)
    r `shouldBe` r')

testRoundTripRF :: forall a . (FromRDF a, ToRDF a, Eq a, Show a, RealFloat a) => [Term] -> SpecWith ()
testRoundTripRF xs = forM_ xs (\x ->
  it ("parses, renders and parses again " <> show x) $ do
    let r  = fromJust $ fromRDF @a x
    let r' = fromJust $ fromRDF @a (toRDF r)
    shouldSatisfy (r, r') (\(a, b) ->
      (isNaN a && isNaN b) || a == b))

testInts :: [Term]
testInts =
    (flip typedL xsdInteger <$> validIntegers)
    <> (flip typedL xsdNonPositiveInteger <$> validIntegers)
    <> (flip typedL xsdInt <$> validIntegers)
  where
    validIntegers = ["-1", "0", "+0", "12678967543233", "+100000"]

testFloats :: [Term]
testFloats =
    (flip typedL xsdFloat <$> validFloats)
    <> (flip typedL xsdDouble <$> validFloats)
  where
    validFloats = ["-1E4", "1267.43233E12", "12.78e-2", "12",  "-0", "0", "INF", "-INF", "NaN"]

testDecimals :: [Term]
testDecimals = flip typedL xsdDecimal <$> validDecimals
  where
    validDecimals = ["-1.23", "12678967.543233", "+100000.00", "210"]
