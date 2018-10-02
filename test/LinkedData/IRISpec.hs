{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module LinkedData.IRISpec where

import           Control.Monad (forM_)
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Data.Monoid ((<>))
import           LinkedData (IRI, Abs, Rel, Prefix(..), commonNamespaces)
import           LinkedData (mkAbsIRI, mkRelIRI, unIRI, relativeTo, relativeFrom, findPrefix)
import           LinkedData.QQ (absiri, reliri)
import           Test.Hspec
import           Test.QuickCheck (property, withMaxSuccess)

spec :: Spec
spec = describe "IRI" $ do
  describe "mkAbsIRI" $
    it "renders and parses a random IRI" $
      withMaxSuccess 20 $ property (\iri ->
        mkAbsIRI (unIRI iri) `shouldBe` Just iri)
  describe "mkRelIRI" $
    it "renders and parses a random IRI" $
      withMaxSuccess 20 $ property (\iri ->
        mkRelIRI (unIRI iri) `shouldBe` Just iri)
  describe "relativeTo" $ do
    context "resolves the examples from RFC 3986 to base 'http://a/b/c/d;p?q'" $
      forM_ relativeToTests (\(r, e) ->
        it ("resolves '" <> T.unpack (unIRI r) <> "'") $
          (r `relativeTo` [absiri|http://a/b/c/d;p?q|]) `shouldBe` Just e)
  describe "relativeFrom" $ do
    context "resolves the following tests" $
      forM_ relativeFromTests (\(b, r, e) ->
        it ("resolves '" <> T.unpack (unIRI r) <> "' to base '" <> T.unpack (unIRI b) <> "'") $
          (r `relativeFrom` b) `shouldBe` Just e)
    it "is consistent with relativeTo" $
      withMaxSuccess 100 $ property (\a base ->
        let r  = fromJust $ a `relativeFrom` base
            a' = fromJust $ r `relativeTo` base
            e  = fromJust $ a `relativeTo` base
        in  a' `shouldBe` e)
  describe "findPrefix" $
    forM_ prefixTests (\(u, e) ->
      it ("finds the prefix and relative IRI for " <> T.unpack (unIRI u)) $
        findPrefix commonNamespaces u `shouldBe` e)


-- | Test cases from section 5.4.1 from RFC 3986.
--
-- First item in the tuple is the relative path, the second is the expected
-- result. The base path is always @http://a/b/c/d;p?q@.
relativeToTests :: [(IRI Rel, IRI Abs)]
relativeToTests =
  [ -- Normal examples
    ([reliri|g:h|],     [absiri|g:h|])
  , ([reliri|g|],       [absiri|http://a/b/c/g|])
  , ([reliri|./g|],     [absiri|http://a/b/c/g|])
  , ([reliri|g/|],      [absiri|http://a/b/c/g/|])
  , ([reliri|/g|],      [absiri|http://a/g|])
  , ([reliri|//g|],     [absiri|http://g|])
  , ([reliri|?y|],      [absiri|http://a/b/c/d;p?y|])
  , ([reliri|g?y|],     [absiri|http://a/b/c/g?y|])
  , ([reliri|#s|],      [absiri|http://a/b/c/d;p?q#s|])
  , ([reliri|g#s|],     [absiri|http://a/b/c/g#s|])
  , ([reliri|g?y#s|],   [absiri|http://a/b/c/g?y#s|])
  , ([reliri|;x|],      [absiri|http://a/b/c/;x|])
  , ([reliri|g;x|],     [absiri|http://a/b/c/g;x|])
  , ([reliri|g;x?y#s|], [absiri|http://a/b/c/g;x?y#s|])
  , ([reliri||],        [absiri|http://a/b/c/d;p?q|])
  , ([reliri|.|],       [absiri|http://a/b/c/|])
  , ([reliri|./|],      [absiri|http://a/b/c/|])
  , ([reliri|..|],      [absiri|http://a/b/|])
  , ([reliri|../|],     [absiri|http://a/b/|])
  , ([reliri|../g|],    [absiri|http://a/b/g|])
  , ([reliri|../..|],   [absiri|http://a/|])
  , ([reliri|../../|],  [absiri|http://a/|])
  , ([reliri|../../g|], [absiri|http://a/g|])
    -- Abnormal cases
  , ([reliri|../../../g|],    [absiri|http://a/g|])
  , ([reliri|../../../../g|], [absiri|http://a/g|])
    -- Dot segments
  , ([reliri|/./g|],    [absiri|http://a/g|])
  , ([reliri|/../g|],   [absiri|http://a/g|])
  , ([reliri|g.|],      [absiri|http://a/b/c/g.|])
  , ([reliri|.g|],      [absiri|http://a/b/c/.g|])
  , ([reliri|g..|],     [absiri|http://a/b/c/g..|])
  , ([reliri|..g|],     [absiri|http://a/b/c/..g|])
    -- Nonsensical forms of the |].|] and |]..|]
  , ([reliri|./../g|],     [absiri|http://a/b/g|])
  , ([reliri|./g/.|],      [absiri|http://a/b/c/g/|])
  , ([reliri|g/./h|],      [absiri|http://a/b/c/g/h|])
  , ([reliri|g/../h|],     [absiri|http://a/b/c/h|])
  , ([reliri|g;x=1/./y|],  [absiri|http://a/b/c/g;x=1/y|])
  , ([reliri|g;x=1/../y|], [absiri|http://a/b/c/y|])
    -- Query and/or fragment components
  , ([reliri|g?y/./x|],  [absiri|http://a/b/c/g?y/./x|])
  , ([reliri|g?y/../x|], [absiri|http://a/b/c/g?y/../x|])
  , ([reliri|g#s/./x|],  [absiri|http://a/b/c/g#s/./x|])
  , ([reliri|g#s/../x|], [absiri|http://a/b/c/g#s/../x|])
  ]

-- | Test cases for 'relativeFrom' function.
relativeFromTests :: [(IRI Abs, IRI Abs, IRI Rel)]
relativeFromTests =
  -- base                     relative                    expected
  [ ([absiri|http://a/b/c|],  [absiri|http://a/b/c|],     [reliri|http://a/b/c|])
  , ([absiri|http://a/b/c|],  [absiri|ftp://a/b/c|],      [reliri|ftp://a/b/c|])   -- scheme does not match
  , ([absiri|http://a/b/c|],  [absiri|http://b/c/d|],     [reliri|http://b/c/d|])  -- host does not match
  , ([absiri|http://a/b/|],   [absiri|http://a/b/c/d/|],  [reliri|c/d/|])          -- path relative to base
  , ([absiri|http://a/b/c|],  [absiri|http://a/b/d|],     [reliri|/b/d|])          -- path relative without trailing slash
  , ([absiri|http://a/b/c/|], [absiri|http://a/b/d|],     [reliri|/b/d|])          -- path relative with trailing slash
  , ([absiri|http://a/b/c/|], [absiri|http://a/b/|],      [reliri|/b/|])           -- path shorter than base
  , ([absiri|http://a/b/c/|], [absiri|http://a/b/c/?q|],  [reliri|?q|])            -- query only
  , ([absiri|http://a/b/c/|], [absiri|http://a/b/c/#s|],  [reliri|#s|])            -- hash only
  , ([absiri|http://a/b/|],   [absiri|http://a/b|],       [reliri|/b|])
  ]

testRelativeTo :: T.Text -> T.Text -> T.Text -> Expectation
testRelativeTo b r e =
  let b' = fromJust $ mkAbsIRI b
      r' = fromJust $ mkRelIRI r
      e' = fromJust $ mkAbsIRI e
  in  (r' `relativeTo` b') `shouldBe` Just e'

prefixTests :: [(IRI Abs, Maybe (Prefix, IRI Rel))]
prefixTests =
  -- URL
  [ ([absiri|http://purl.org/dc/elements/1.1/title|],           Just (Prefix "dc",  [reliri|title|]))
  , ([absiri|http://www.w3.org/1999/02/22-rdf-syntax-ns#type|], Just (Prefix "rdf", [reliri|type|]))
  , ([absiri|http://www.example.com/foo#bar|],                  Nothing)
  ]
