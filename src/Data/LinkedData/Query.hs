module Data.LinkedData.Query
  ( query
  , select
  )
where

import           Data.Foldable (foldl')
import           Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Map.Strict as M
import           Data.LinkedData.Types
import           Data.LinkedData.Utils (subjectOf, predicateOf, objectOf, isQueryVar)

-- | Simple query
query :: Graph -> Term -> Term -> Term -> Triples
query Graph { triples=ts } s p o =
    filter (\(Triple s' p' o') -> matchTerm s s' && matchTerm p p' && matchTerm o o') ts
  where
    matchTerm :: Term -> Term -> Bool
    matchTerm (Var _) _ = True
    matchTerm a       b = a == b

-- | Advanced query
--
-- TODO: this is the most naive algorithm to come up with, we need a query
-- optimizer. The simplest optimization would be to sort the clauses in order of
-- the one that matches the least triples up to the one that matches the most
-- triples.
select :: Graph     -- ^ graph to query
       -> [Term]    -- ^ select parameters
       -> Triples   -- ^ clauses
       -> [[Term]]  -- ^ list of results
select _ _ [] = []
select g sps (clause : clauses) =
    bindingsToList <$> foldl' bindings (initialBindings clause) clauses
  where
    bindingsToList :: M.Map Term Term -> [Term]
    bindingsToList b = (\s -> fromMaybe s (M.lookup s b)) <$> sps

    initialBindings :: Triple -> [M.Map Term Term]
    initialBindings c@(Triple s p o) = mapMaybe (binding vars M.empty) ts
      where
        ts = query g s p o
        vars = clauseVars c

    -- | Eliminate bindings that do not work for this clause.
    bindings :: [M.Map Term Term]  -- ^ bindings found in previous step
             -> Triple             -- ^ clause to process
             -> [M.Map Term Term]  -- ^ new bindings that match the clause
    bindings bs c@(Triple s p o) = concatMap (\b -> mapMaybe (binding vars b) ts) bs
      where
        ts = query g s p o
        vars = clauseVars c

    binding :: [(Term, Triple -> Term)]  -- ^ variables of clause
            -> M.Map Term Term           -- ^ binding found in previous step
            -> Triple                    -- ^ triple to process
            -> Maybe (M.Map Term Term)   -- ^ optional binding found
    binding []                b _ = pure b
    binding ((var, f) : vars) b t =
      case M.lookup var b of
        Just bind ->
          if bind /= f t
            then Nothing
            else binding vars b t
        Nothing -> do
          let b' = M.insert var (f t) b
          binding vars b' t

    -- | This is a hack to easily store the variables with their position in the
    -- triple. We store the function to extract either s, p or o from the
    -- triple with the variable.
    clauseVars :: Triple                    -- ^ clause
               -> [(Term, Triple -> Term)]  -- ^ variables of clause
    clauseVars (Triple s p o)  = catMaybes [
        if isQueryVar s then Just (s, subjectOf)   else Nothing
      , if isQueryVar p then Just (p, predicateOf) else Nothing
      , if isQueryVar o then Just (o, objectOf)    else Nothing ]
