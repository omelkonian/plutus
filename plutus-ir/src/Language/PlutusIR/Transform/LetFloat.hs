{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module LetFloat (floatMergeLets) where

import           Language.PlutusIR
import           Language.PlutusIR.Analysis.ScopeGraph

import           Control.Lens
import           Control.Monad.Trans.State

import qualified Language.PlutusCore                   as PLC
import qualified Language.PlutusCore.Name              as PLC

import qualified Algebra.Graph.AdjacencyMap            as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm  as AM (isAcyclic)
import qualified Data.Map                              as M


separateLets :: (PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
                    => Term tyname name a
                    -> (Term tyname name a
                       ,M.Map PLC.Unique (a, Recursivity, Binding tyname name a)
                       )
separateLets = flip runState M.empty . separateLets'

-- | TODO: move it inside 'separateLets'
separateLets' :: (Monad m, PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
              => Term tyname name a
              -> StateT (M.Map PLC.Unique (a, Recursivity, Binding tyname name a)) m (Term tyname name a)
separateLets' = \case
      -- this overrides the 'termSubterms' functionality only for the 'Let' constructor
      Let a r bs t' -> do

        bs' <- traverse (bindingSubterms separateLets') bs

        let newMap = M.fromList $
                            fmap (\ b -> (bindingUnique b, (a,r,b))) bs'

        modify (M.union newMap)

        separateLets' t'

      t -> termSubterms separateLets' t


-- | The compiler letfloat/letmerge pass
-- TODO:
floatMergeLets :: (PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
         => Term tyname name a
         -> Term tyname name a
floatMergeLets t =
  let scopeGraph = runTermDeps t :: AM.AdjacencyMap Node
      Just transScopeGraph = transitiveReduction scopeGraph
      (noLetsTerm, lets) = separateLets t
  in floatMergeLets' transScopeGraph noLetsTerm lets

floatMergeLets' :: (PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
                => AM.AdjacencyMap Node
                -> Term tyname name a
                -> M.Map PLC.Unique (a, Recursivity, Binding tyname name a)
                -> Term tyname name a
floatMergeLets' g t ls = undefined



-- HELPERS
----------

-- | return a single 'Unique' for a particular binding.
-- TODO: maybe remove this boilerplate by having a lens "name" for a Binding
bindingUnique :: (PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique) => Binding tyname name a -> PLC.Unique
bindingUnique = \case TermBind _ _ (VarDecl _ n _) _ -> n ^. PLC.unique . coerced
                      TypeBind _ (TyVarDecl _ n _) _ -> n ^. PLC.unique . coerced
                      DatatypeBind _ _ -> error "TODO: not implemented yet"


-- | Implementation taken from: https://github.com/snowleopard/alga/issues/165
--
-- It uses the 'AM.transitiveClosure' to compute the transitive reduction.
-- Note: there may be a faster implementation by using 'AM.dfsForest'
-- TODO: maybe move it into a separate helper module
transitiveReduction :: Ord a => AM.AdjacencyMap a -> Maybe (AM.AdjacencyMap a)
transitiveReduction g
    | not (AM.isAcyclic g) = Nothing
    | otherwise         = Just $ AM.vertices vs `AM.overlay` AM.edges es
  where
    vs = AM.vertexList g
    es = filter (not . transitive) (AM.edgeList g)
    jumpGraph         = let t = AM.transitiveClosure g in AM.compose t t
    transitive (x, y) = AM.hasEdge x y jumpGraph
