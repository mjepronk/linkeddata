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
-- - remove uses of fromJust

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module LinkedData.Graphs
  ( isIsomorphic
  , isIsomorphicWithDiff
  , canonicaliseBlankNodes
  , skolemiseGraphCanonical
  )
where

import           Data.List ((\\))
import           Data.Monoid (Monoid(..), (<>))
import           Data.Maybe (fromJust)
import           Data.Foldable (foldl')
import           Data.Hashable (Hashable(..))
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import           GHC.Generics (Generic)
import           LinkedData.Types (Graph(..), Triple(..), Triples, Term(..))
import           LinkedData.IRI (IRI, Abs, mkRelIRI, (.:.))
import           LinkedData.Utils (isBlank)


-- | A 'Colour' is a hash value (named after the term used in the paper).
newtype Colour = Clr Int
    deriving (Eq, Show)

instance Hashable Colour where
    hash (Clr a) = a
    hashWithSalt salt (Clr a) = salt `hashWithSalt` a

instance Semigroup Colour where
    (Clr a) <> (Clr b) = Clr (a + b)

instance Monoid Colour where
    mempty = Clr 0

-- | Direction in the graph (in the paper symbolized by '+' and '-').
data Direction = Plus | Minus deriving Generic

instance Hashable Direction


-- | Map from RDF Term to 'Colour' (hash)
type ColourMap = M.Map Term Colour

hashC :: Hashable a => a -> Colour
hashC = Clr . hash

-- | Test if two graphs are isomorphic (equal except for the labels of the blank
-- nodes).
isIsomorphic :: Graph -> Graph -> Bool
isIsomorphic a b = hashGraph a == hashGraph b


-- | Test if two graphs are isomorphic, but if this is not the case returns the
-- triples that are in graph 'a' but not in 'b', and vice versa.
--
-- TODO: reconvert to original BNode's in diffA and diffB
isIsomorphicWithDiff :: Graph -> Graph -> Either (Triples, Triples) ()
isIsomorphicWithDiff a b =
   let a' = canonicaliseBlankNodes a
       b' = canonicaliseBlankNodes b
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
skolemiseGraphCanonical :: IRI Abs -> Graph -> Graph
skolemiseGraphCanonical base g@Graph { triples } = g{ triples=skolTriple <$> triples}
  where
    clr = colour True g
    ghash = hashGraphWithClr clr g
    skolTriple (Triple s p o) = Triple (skolTerm s) p (skolTerm o)
    skolTerm t
        | isBlank t =
            let h = hash (fromJust (M.lookup t clr), ghash)
                r = fromJust (mkRelIRI ("/.well-known/genid/canonical/" <> T.pack (show h)))
            in  ITerm (base .:. r)
        | otherwise     = t

-- | Convert skolemised IRI's to blank nodes.
-- deskolemiseGraph :: Graph -> Graph
-- deskolemiseGraph _ = undefined


-- TODO
-- leanGraph :: Graph -> Graph


-- | Hash a graph.
hashGraph :: Graph -> Colour
hashGraph g = hashGraphWithClr (colour True g) g

-- | Hash a graph with a precomputed colour map.
hashGraphWithClr :: ColourMap -> Graph -> Colour
hashGraphWithClr clr Graph { triples } = mconcat (hashTriple <$> triples)
  where
    hashTriple (Triple s p o) = hashC (getColour s, getColour p, getColour o)
    getColour t = fromJust (M.lookup t clr)

-- | Get a canonical representation (that remains isomorphic for isomorphic
-- graphs) for every blank node based on the terms that surround it.
canonicaliseBlankNodes :: Graph -> Graph
canonicaliseBlankNodes g@Graph { triples } = g { triples = canonTriple <$> triples }
  where
    clr = colour False g
    canonTriple (Triple s p o) = Triple (canonTerm s) p (canonTerm o)
    canonTerm t
        | isBlank t =
            let (Just (Clr i)) = M.lookup t clr
            in  BNodeGen i
        | otherwise = t

-- | Create an initial 'ColourMap' (map from term to hash).
initColourMap :: [Term]     -- ^ Terms of Graph to partition
              -> ColourMap  -- ^ Map from terms to colour (hash)
initColourMap = foldl' go M.empty
  where
    go :: ColourMap -> Term -> ColourMap
    go clr t
        | isBlank t = M.insert t mempty clr
        | otherwise = M.insert t (hashC t) clr

-- | Create a colour map (map from term to hash) for a graph.
colour :: Bool       -- ^ Hash all terms or blank nodes only?
       -> Graph      -- ^ Graph
       -> ColourMap  -- ^ Map from term to colour (hash)
colour allTerms g =
    let btriples = filter (\(Triple s _ o) -> isBlank s || isBlank o) (triples g)
        bnodes = filter isBlank $ concatMap tripleToList btriples
        clr = initColourMap (concatMap tripleToList (if allTerms then (triples g) else btriples))
    in  go clr btriples bnodes
  where
    go :: ColourMap -> Triples -> [Term] -> ColourMap
    go clr ts bnodes =
        let tmp  = foldl' (foldSubjects clr) clr ts
            clr' = foldl' (foldObjects  clr) tmp ts
            isPartitionUnchanged = and [pairUnchanged clr clr' x y | x <- bnodes, y <- bnodes, x /= y]
        in  if isPartitionUnchanged
                then clr'
                else go clr' ts bnodes

-- | The functions used to fold over Subjects and Objects
foldSubjects, foldObjects
    :: ColourMap  -- ^ Colour map from previous iteration
    -> ColourMap  -- ^ Colour map to be updated
    -> Triple     -- ^ Triple to process
    -> ColourMap  -- ^ Updated colour map
foldSubjects clr clr' (Triple b p o)
    | isBlank b = M.insert b (hashC (p', o', Plus) <> b') clr'
    | otherwise = clr'
  where
    b' = fromJust $ M.lookup b clr'
    p' = fromJust $ M.lookup p clr
    o' = fromJust $ M.lookup o clr

foldObjects clr clr' (Triple s p b)
    | isBlank b = M.insert b (hashC (s', p', Minus) <> b') clr'
    | otherwise = clr'
  where
    b' = fromJust $ M.lookup b clr'
    s' = fromJust $ M.lookup s clr
    p' = fromJust $ M.lookup p clr

-- | Check if a pair of terms has changed between two iterations of Colour.
pairUnchanged :: ColourMap -> ColourMap -> Term -> Term -> Bool
pairUnchanged clr clr' x y =
    (fromJust (M.lookup x clr') /= fromJust (M.lookup y clr')) ||
    (fromJust (M.lookup x clr)  == fromJust (M.lookup y clr))

-- | Triple to list of terms.
tripleToList :: Triple -> [Term]
tripleToList (Triple s p o) = [s, p, o]
