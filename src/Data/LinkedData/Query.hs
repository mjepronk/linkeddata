{-# LANGUAGE NamedFieldPuns #-}
module Data.LinkedData.Query where

-- | See: http://aidanhogan.com/docs/graph_database_query_survey.pdf

import Data.Maybe (catMaybes, fromMaybe)
import Data.Traversable (for)
import qualified Data.Map.Strict as M
import Data.LinkedData.Types
import Data.LinkedData.Utils (subjectOf, predicateOf, objectOf, isQueryVar)

-- | Simple query
query :: Graph -> Term -> Term -> Term -> Triples
query Graph { triples } s p o =
    filter (\(Triple s' p' o') -> matchTerm s s' && matchTerm p p' && matchTerm o o') triples
  where
    matchTerm :: Term -> Term -> Bool
    matchTerm (Var _) _ = True
    matchTerm a       b = a == b

-- | Advanced query
select :: Graph     -- ^ graph to query
       -> [Term]    -- ^ select parameters
       -> Triples   -- ^ clauses
       -> [[Term]]  -- ^ list of results
select _ _ [] = []
select Graph { triples } selectP (firstC : clauses) =
    bindingsToList selectP <$> go triples clauses (initialBindings firstC triples)
  where

    -- TODO: rewrite as fold!?
    go :: Triples            -- ^ triples to process
       -> Triples            -- ^ clauses
       -> [M.Map Term Term]  -- ^ bindings found thus far
       -> [M.Map Term Term]  -- ^ list of bindings as map from query Var to the bound Term
    go _  []                      bs = bs
    go ts (c : cs) bs = go cs ts (bindings c ts bs)

    bindingsToList :: [Term] -> M.Map Term Term -> [Term]
    bindingsToList sps b = (\s -> fromMaybe s (M.lookup s b)) <$> sps

    clauseVars :: Triple                    -- ^ clause
               -> [(Term, Triple -> Term)]  -- ^ variables of clause
    clauseVars (Triple s p o)  = catMaybes [
          if isQueryVar s then Just (s, subjectOf)   else Nothing
        , if isQueryVar p then Just (p, predicateOf) else Nothing
        , if isQueryVar o then Just (o, objectOf)    else Nothing ]

    initialBindings :: Triple -> Triples -> [M.Map Term Term]
    initialBindings c ts = concat $ for ts (binding (clauseVars c) M.empty)

    bindings :: Triple             -- ^ clause to process
             -> Triples            -- ^ triples to process
             -> [M.Map Term Term]  -- ^ bindings found thus far
             -> [M.Map Term Term]  -- ^ updated bindings
    bindings c ts bs = concat . concat $ for ts (\t -> for bs (\b -> binding (clauseVars c) b t))

    binding :: [(Term, Triple -> Term)]  -- ^ variables of clause
            -> M.Map Term Term           -- ^ binding found for a particular triple
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
