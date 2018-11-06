module LinkedData.Query
  ( query
  , select
  )
where

import           Data.Foldable (foldl')
import           Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as M
import           LinkedData.Types

-- | Simple query
query :: Graph -> TriplePattern -> Triples
query Graph { triples=ts } (TriplePattern s p o) =
    filter (\(Triple s' p' o') -> matchTerm s s' && matchTerm p p' && matchTerm o o') ts
  where
    matchTerm :: VarOrTerm -> Term -> Bool
    matchTerm (Left _) _  = True   -- Var
    matchTerm (Right a) b = a == b -- Term

-- | Advanced query
select :: Graph           -- ^ graph to query
       -> [Var]           -- ^ select parameters
       -> [TriplePattern] -- ^ clauses
       -> [[Term]]        -- ^ list of results (the list of bound terms in order)
select _ _ [] = []
select g vs (x : xs) =
    bindingsToList <$> foldl' bindings (initialBindings x) xs
  where
    bindingsToList :: M.Map Var Term -> [Term]
    bindingsToList b = mapMaybe (flip M.lookup b) vs

    initialBindings :: TriplePattern -> [M.Map Var Term]
    initialBindings tp = mapMaybe (binding M.empty tp) (query g tp)

    -- | Eliminate bindings that do not work for this clause.
    bindings :: [M.Map Var Term]  -- ^ bindings found in previous step
             -> TriplePattern     -- ^ clause to process
             -> [M.Map Var Term]  -- ^ new bindings that match the clause
    bindings bs tp = concatMap (\b -> mapMaybe (binding b tp) (query g tp)) bs

    -- | Get binding for a single triple.
    binding :: M.Map Var Term  -- ^ bindings found in previous step
            -> TriplePattern   -- ^ clause to process
            -> Triple          -- ^ triple to process
            -> Maybe (M.Map Var Term)  -- ^ binding found
    binding b (TriplePattern ps pp po) (Triple s p o) = do
        bs <- binding' b  ps s
        bp <- binding' bs pp p
        bo <- binding' bp po o
        pure bo
      where
        -- | Get binding for a single term.
        binding' :: M.Map Var Term -> VarOrTerm -> Term -> Maybe (M.Map Var Term)
        binding' b' (Left var) t =
          case M.lookup var b' of
            Just t'
              | t' == t   -> pure b'
              | otherwise -> Nothing
            Nothing -> pure (M.insert var t b')
        binding' b' (Right _) _ = pure b'
