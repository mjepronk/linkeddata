{-# LANGUAGE QuasiQuotes #-}

module LinkedData.GraphsSpec
  ( spec )
where

import Test.HSpec

import LinkedData (Graph, triple, foafNS, (.:.))
import LinkedData.QQ (reliri)


spec :: Spec
spec = do
    describe "Hashable instances" $
        it "hash of same terms "
        it "hashes different terms differently" $
            hash (ITerm [absiri|http://example.com]) `shouldNotBe` hash (plainL "http://example.com")

    describe "isIsomorphic" $
        it "positively identifies trivial graph" $
            let g  = Graph { triples=[ triple (BNode "a") foafKnows (BNode "b") ]}
                g' = Graph { triples=[ triple (BNode "x") foafKnows (BNode "y") ]}
            in  g `isIsomorphic` g' `shouldBe` True
        it "positively identifies simple graph" $
            let g  = Graph { triples=
                        [ triple (BNode "a") foafKnows (BNode "b")
                        , triple (BNode "b") foafKnows (BNode "c")
                        ]}
                g' = Graph { triples=
                        [ triple (BNode "x") foafKnows (BNode "y")
                        , triple (BNode "y") foafKnows (BNode "z")
                        ]}
            in  g `isIsomorphic` g' `shouldBe` True

foafKnows :: Term
foafKnows = faofNS .:. [reliri|knows|]
