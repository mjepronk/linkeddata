-- | Functions for manipulating graphs.
--
-- See:
-- - Aidan Hogan. "Skolemising Blank Nodes while Preserving Isomorphism". In
--   the Proceedings of the 24th International World Wide Web Conference (WWW),
--   Florence, Italy, May 18–22, 2015
--   http://aidanhogan.com/docs/skolems_blank_nodes_www.pdf
--
-- TODO:
-- - implement the complete algorithm from the paper (handle automorphism)
-- - create a bijection, which would serve as the basis for a diff tool
-- - use Int64 as hash ? use Digest from cryptonite as hash ?
--   (which is possible to convert to a bytearray that supports for example xor...)
-- - use XOR instead of (+) ?


{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.LinkedData.Graphs
  ( isIsomorphic
  , isIsomorphicWithDiff
  , canonicaliseBlankNodes
  , skolemiseGraphCanonical
  )
where

import           Data.List ((\\))
import           Data.Monoid ((<>))
import           Data.Maybe (fromJust)
import           Data.Foldable (foldl')
import           Data.Binary (encode, decode)
import qualified Data.Hashable as H
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import           Data.LinkedData.Types (Graph(..), Triple(..), Triples, Term(..), IRI(..))
import           Data.LinkedData.Utils (isBlankNode)


-- | Map from RDF Term to colour (hash)
type ColourMap = M.Map Term Int

-- | Hash function interface
data HashF = HashF {
    -- | Empty hash.
    hashNull :: Int

    -- | Hash a single RDF term.
  , hashTerm :: Term -> Int

    -- | Hash a char.
  , hashChar :: Char -> Int

    -- | Hash a list of binary encodable data, with a normal hash function.
  , hashCombineOrdered :: Int -> Int -> Int

    -- | Hash a list of binary encodable data, with a commutative and
    -- associative hash function. Note that this function is parallelisable.
  , hashCombineUnordered :: Int -> Int -> Int
  }

-- TODO: revise these...
defaultHashF :: HashF
defaultHashF = HashF {
      hashNull = H.hash B.empty
    , hashTerm = H.hash . BL.toStrict . encode
    , hashChar = H.hash . BL.toStrict . encode
    , hashCombineOrdered = \a b -> H.hash $ (BL.toStrict . encode $ a) <> (BL.toStrict . encode $ b)
    , hashCombineUnordered = \a b -> decode . BL.pack $ B.zipWith (+) (BL.toStrict . encode $ a) (BL.toStrict . encode $ b)
    }

naiveHashF :: HashF
naiveHashF = HashF {
      hashNull = 0
    , hashTerm = H.hash . BL.toStrict . encode
    , hashChar = H.hash
    , hashCombineOrdered = \a b -> H.hash (a + b)
    , hashCombineUnordered = (+)
    }

-- sha256HashF :: HashF
-- sha256HashF = HashF {}


-- | Test if two graphs are isomorphic (equal except for the labels of the blank
-- nodes).
isIsomorphic :: Graph -> Graph -> Bool
isIsomorphic a b = (hashGraph defaultHashF a) == (hashGraph defaultHashF b)


-- | Test if two graphs are isomorphic, but if this is not the case returns the
-- triples that are in graph 'a' but not in 'b', and vice versa.
--
-- TODO: reconvert to original BNode's in diffA and diffB
isIsomorphicWithDiff :: Graph -> Graph -> Either (Triples, Triples) ()
isIsomorphicWithDiff a b =
   let a' = canonicaliseBlankNodes defaultHashF a
       b' = canonicaliseBlankNodes defaultHashF b
       diffA = triples a' \\ triples b'
       diffB = triples b' \\ triples a'
   in  if null diffA && null diffB
         then pure ()
         else Left (diffA, diffB)


-- | Skolemise a RDF graph by assigning a unique UUID to every blank node.
-- skolemiseGraphUUID :: Graph -> Graph
-- skolemiseGraphUUID = undefined

-- | Skolemise a RDF graph so that the output graph is equal to another
-- skolemised graph if and only if the input graphs are isomorphic.
skolemiseGraphCanonical :: HashF -> T.Text -> Graph -> Graph
skolemiseGraphCanonical hf urlprefix (g@Graph { triples }) = g{ triples=skolTriple <$> triples}
  where
    clr = colour hf True g
    ghash = hashGraphWithClr hf clr g
    skolTriple (Triple s p o) = Triple (skolTerm s) p (skolTerm o)
    skolTerm t
        | isBlankNode t = ITerm . IRI $ urlprefix <> "/.well-known/genid/canonical/" <> (T.pack . show $ h)
        | otherwise     = t
        where h = (hashCombineOrdered hf) (fromJust (M.lookup t clr)) ghash

-- | Convert skolemised IRI's to blank nodes.
-- deskolemiseGraph :: Graph -> Graph
-- deskolemiseGraph _ = undefined


-- TODO
-- leanGraph :: Graph -> Graph


-- | Hash a graph.
hashGraph :: HashF -> Graph -> Int
hashGraph hf g = hashGraphWithClr hf clr g
  where
    clr = colour hf True g

-- | Internal, hash a graph with a precomputed colour map.
hashGraphWithClr :: HashF -> ColourMap -> Graph -> Int
hashGraphWithClr hf clr Graph { triples } =
    foldl' (hashCombineUnordered hf) (hashNull hf) (hashTriple <$> triples)
  where
    ho = hashCombineOrdered hf
    hashTriple (Triple s p o) = ho (ho (getColour s) (getColour p)) (getColour o)
    getColour t = fromJust (M.lookup t clr)


-- | Get a canonical representation (that remains isomorphic for isomorphic
-- graphs) for every blank node based on the terms that surround it.
canonicaliseBlankNodes :: HashF -> Graph -> Graph
canonicaliseBlankNodes hf (g@Graph { triples }) = g { triples = canonTriple <$> triples }
  where
    clr = colour hf False g
    canonTriple (Triple s p o) = Triple (canonTerm s) p (canonTerm o)
    canonTerm t
        | isBlankNode t = BNodeGen $ fromJust $ M.lookup t clr
        | otherwise     = t


-- | Create a colour map (map from term to hash) for a graph.
colour :: HashF      -- ^ Hash function
       -> Bool       -- ^ Hash all terms or blank nodes only?
       -> Graph      -- ^ Graph
       -> ColourMap  -- ^ Map from term to colour (hash)
colour hf allTerms g =
    let btriples = filter (\(Triple s _ o) -> isBlankNode s || isBlankNode o) (triples g)
        bnodes = filter isBlankNode $ concatMap tripleToList btriples
        clr = initColourMap (concatMap tripleToList (if allTerms then (triples g) else btriples))
    in  go 1 clr btriples bnodes
  where
    go :: Int -> ColourMap -> Triples -> [Term] -> ColourMap
    go i clr ts bnodes =
        let tmp  = foldl' (foldSubjects clr) clr ts
            clr' = foldl' (foldObjects  clr) tmp ts
            isPartitionUnchanged = and [pairUnchanged clr clr' x y | x <- bnodes, y <- bnodes, x /= y]
        in  if isPartitionUnchanged
                then clr'
                else go (i + 1) clr' ts bnodes

    initColourMap :: [Term]     -- ^ Terms of Graph to partition
                  -> ColourMap  -- ^ Map from terms to colour (hash)
    initColourMap = foldl' go' M.empty
        where
            go' :: ColourMap -> Term -> ColourMap
            go' clr t
                | isBlankNode t = M.insert t (hashNull hf) clr
                | otherwise     = M.insert t ((hashTerm hf) t) clr

    foldSubjects, foldObjects :: ColourMap  -- ^ Colour map from previous iteration
                              -> ColourMap  -- ^ Colour map to be updated
                              -> Triple     -- ^ Triple to process
                              -> ColourMap  -- ^ Updated colour map
    foldSubjects clr clr' (Triple b p o)
        | isBlankNode b = M.insert b (hu (ho (ho p' o') symbol) b') clr'
        | otherwise     = clr'
      where
        b' = fromJust $ M.lookup b clr'
        p' = fromJust $ M.lookup p clr
        o' = fromJust $ M.lookup o clr
        ho = hashCombineOrdered hf
        hu = hashCombineUnordered hf
        symbol = (hashChar hf) '+'

    foldObjects clr clr' (Triple s p b)
        | isBlankNode b = M.insert b (hu (ho (ho s' p') symbol) b') clr'
        | otherwise     = clr'
      where
        b' = fromJust $ M.lookup b clr'
        s' = fromJust $ M.lookup s clr
        p' = fromJust $ M.lookup p clr
        ho = hashCombineOrdered hf
        hu = hashCombineUnordered hf
        symbol = (hashChar hf) '-'

    pairUnchanged :: ColourMap -> ColourMap -> Term -> Term -> Bool
    pairUnchanged clr clr' x y =
        (fromJust (M.lookup x clr') /= fromJust (M.lookup y clr')) ||
        (fromJust (M.lookup x clr)  == fromJust (M.lookup y clr))


-- | Triple to list of terms.
tripleToList :: Triple -> [Term]
tripleToList (Triple s p o) = [s, p, o]
