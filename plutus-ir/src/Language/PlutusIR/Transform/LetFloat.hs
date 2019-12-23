{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Language.PlutusIR.Transform.LetFloat (float) where

import           Language.PlutusIR
import           Language.PlutusIR.Analysis.ScopeGraph

import           Control.Lens
import           Control.Monad.Trans.State
import Data.Maybe (fromJust)

import qualified Language.PlutusCore                   as PLC
import qualified Language.PlutusCore.Name              as PLC

import qualified Algebra.Graph.AdjacencyMap            as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm  as AM (isAcyclic)
import qualified Data.Map                              as M
import qualified Data.Set as S

import Data.Text (Text)

import Language.PlutusIR.Parser


type LetTable tyname name a = M.Map Node (a, Recursivity, Binding tyname name a)

extractLets :: (PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
                    => Term tyname name a
                    -> (Term tyname name a
                       ,LetTable tyname name a
                       )
extractLets = flip runState M.empty . extractLets'
 where
    extractLets' :: (Monad m, PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
                  => Term tyname name a
                  -> StateT (LetTable tyname name a) m (Term tyname name a)
    extractLets' = \case
          -- this overrides the 'termSubterms' functionality only for the 'Let' constructor
          Let a r bs t' -> do

            bs' <- traverse (bindingSubterms extractLets') bs

            let newMap = M.fromList $
                                fmap (\ b -> (LetOrFree $  bindingUnique b, (a,r,b))) bs'

            modify (M.union newMap)

            extractLets' t'

          t -> termSubterms extractLets' t


-- | The compiler letfloat/letmerge pass
--
-- 1. Compute the scope-graph and transposed scope-graph of the term.
-- 2. Extract all let-bindings from the term and put them in a table.
-- 3. from the current lambda-node (Top in the 1st iter.), take a list of its outgoing a. lambda nodes and b. let nodes
-- 2. remove the (b) let-nodes from the state, and apply (3) for each lambda-node in (a)
-- 3. generate multilet levels where the 1st multilet level is the (b) set, while removing every let that we generate from the state.
-- 5. the resulting top-term has floated some dependent lets. The resulting state contains the let nodes that have not been visited/generated.
-- 6. Starting from ROOT this time, generate multilet levels for the unvisited nodes, and wrap the top-term of (5) with these levels. This means that these lets are floated outermost at the toplevel.
float :: (PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique, Eq a, Ord (tyname a), Ord (name a))
         => Term tyname name a
         -> Term tyname name a
float topTerm =
  let (topLamNodes, _) = separateLamLetNodes $ AM.postSet Top gT -- don't care about lets from Top, no lets can appear linked to top
      (topTermClean, letTable) = extractLets topTerm -- (2)
  in evalState (visit topLamNodes topTermClean
               >>= genLetFree
               ) letTable

  where
    -- the scopegraph
    g = fromJust . transitiveReduction $ runTermDeps topTerm
    -- the transpoeed scopegraph
    gT = AM.transpose g

    visit lamNodes = \case --  (3)
       -- this overrides the 'termSubterms' functionality only for the small and big lambda constructors
      LamAbs a n ty t | toLambdaNode n `S.member` lamNodes -> LamAbs a n ty <$> visitSmallBigLambda (toLambdaNode n) t
      TyAbs a n ty t  | toLambdaNode n `S.member` lamNodes -> TyAbs a n ty <$> visitSmallBigLambda (toLambdaNode n) t
      t -> termSubterms (visit lamNodes) t

    -- | lambdas/Lambdas are visited in the same way.
    visitSmallBigLambda n t =  do
      let (outLamNodes, outLetNodes) = separateLamLetNodes $ AM.postSet n gT
      letTable <- get
      let thisLvl =  M.restrictKeys letTable outLetNodes
      put $ letTable `M.difference` thisLvl -- remove the direct outgoing let-nodes

      t' <- visit outLamNodes t
      genLetLvl thisLvl t'


    -- | Generate 1 multi-let for the current level of lets and recurse for all their let-descendants, grouped in a single level.
    genLetLvl thisLvl t | thisLvl == M.empty = pure t -- no let left to generate
                        | otherwise = do
      letTable <- get

      -- next level = all the outgoing let-nodes of the current let-nodes MINUS the already visited let-nodes
      let nextLvl = M.restrictKeys letTable $
                    foldMap
                    (S.filter (`M.member` letTable) . flip AM.postSet gT) -- filter the postset by members of lettable
                    (M.keys thisLvl)

      put $ letTable `M.difference` nextLvl -- remove the next-level outgoing let-nodes

      Let undefined NonRec (fmap (\(_,_,x)->x) $ M.elems thisLvl) <$> genLetLvl nextLvl t

    -- | perhaps this can be done faster if we start from Root and go backwards
    genLetFree t = do
      letTable <- get
      let (topLvl, restTable) = M.partitionWithKey
                                (const . S.null . flip AM.postSet g)  -- the starting lets must be "LEAVES" in the original (non-transposed) forest/graph
                                letTable
      put restTable
      genLetLvl topLvl t


-- HELPERS
----------

-- | return a single 'Unique' for a particular binding.
-- TODO: maybe remove this boilerplate by having a lens "name" for a Binding
bindingUnique :: (PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique) => Binding tyname name a -> (PLC.Unique,Text)
bindingUnique = \case TermBind _ _ (VarDecl _ n _) _ -> (n ^. PLC.unique . coerced, n ^. PLC.str)
                      TypeBind _ (TyVarDecl _ n _) _ -> (n ^. PLC.unique . coerced, n ^. PLC.str)
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

toLambdaNode :: (PLC.HasUnique s unique) => s -> Node
toLambdaNode n = SmallOrBigLambda (n ^. PLC.unique . coerced, n ^. PLC.str)


-- | Sepeparates a node-set into two sets of lambda/Lambda nodes and Let nodes.
-- Root and Top nodes are removed/ignored.
separateLamLetNodes :: S.Set Node -> (S.Set Node, S.Set Node)
separateLamLetNodes = foldl (\acc@(lams,lets) ->
                                             \case n@(SmallOrBigLambda _) -> (S.insert n lams,lets)
                                                   n@(LetOrFree _) -> (lams,S.insert n lets)
                                                   _ -> acc)
           (S.empty,S.empty) -- TODO: optimize fold?

