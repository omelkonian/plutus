{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedStrings       #-}
-- | Functions for computing the dependency graph of variables within a term or type. A "dependency" between
-- two nodes "A depends on B" means that B cannot be removed from the program without also removing A.
module Language.PlutusIR.Analysis.ScopeGraph (Node (..), DepGraph, runTermDeps, runTypeDeps) where

import qualified Language.PlutusCore               as PLC
import qualified Language.PlutusCore.Name          as PLC

import           Language.PlutusIR
import qualified Language.PlutusIR.Analysis.Usages as Usages

import           Control.Lens
import           Control.Monad.Reader

import qualified Algebra.Graph.Class               as G
import qualified Data.Set                          as Set
import qualified Data.Map as M
import Control.Monad.Trans.State

import qualified Algebra.Graph.AdjacencyMap               as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm            as AM (isAcyclic)
import qualified Data.Text

import Language.PlutusIR.Transform.Rename () -- for instance

-- | A node in a dependency graph. Either a specific 'PLC.Unique', or a specific
-- node indicating the root of the graph. We need the root node because when computing the
-- dependency graph of, say, a term, there will not be a binding for the term itself which
-- we can use to represent it in the graph.
data Node = LetOrFree (PLC.Unique,Data.Text.Text)
          | SmallOrBigLambda (PLC.Unique,Data.Text.Text)
          | Root
          | Top
          deriving (Show, Eq, Ord)

-- | A constraint requiring @g@ to be a 'G.Graph' (so we can compute e.g. a @Relation@ from it), whose
-- vertices are 'Node's.
type DepGraph g = (G.Graph g, (G.Vertex g)~Node)

-- | Compute the dependency graph of a 'Term'. The 'Root' node will correspond to the term itself.
--
-- For example, the graph of @[(let (nonrec) (vardecl x t) y) [x z]]@ is
-- @
--     ROOT -> x
--     ROOT -> z
--     x -> y
--     x -> t
-- @
runTermDeps
    :: (DepGraph g, PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
    => Term tyname name a
    -> g
runTermDeps t = runReader (termDeps t) (Root,[])

-- the reader context enhanced by a full enclosing lambda scope
type Ctx = (Node, Scope)
type Scope = [(PLC.Unique,Data.Text.Text)]

-- | Compute the dependency graph of a 'Type'. The 'Root' node will correspond to the type itself.
--
-- This graph will always be simply a star from 'Root', since types have no internal let-bindings.
--
-- For example, the graph of @(all a (type) [f a])@ is:
-- @
--     ROOT -> f
--     ROOT -> a
-- @
--
runTypeDeps
    :: (DepGraph g, PLC.HasUnique (tyname a) PLC.TypeUnique)
    => Type tyname a
    -> g
runTypeDeps t = runReader (typeDeps t) (Root,[])


-- | Record some dependencies on the current node.
recordDeps
    :: (DepGraph g, MonadReader Ctx m)
    => [(PLC.Unique,Data.Text.Text)]
    -> m g
recordDeps us = do
    (current,scope) <- ask
    pure $ G.connect (G.vertices [current]) (G.vertices (fmap (\ v -> (if v `elem` scope then SmallOrBigLambda else LetOrFree) v) us))

-- | Process the given action with the given name as the current node.
withLet
    :: (MonadReader Ctx m, PLC.HasUnique n u)
    => n
    -> m g
    -> m g
withLet n = local $ set _1 $ LetOrFree (n ^. PLC.unique . coerced, n ^. PLC.str)

withScope
    :: (MonadReader Ctx m, PLC.HasUnique n u)
    => n
    -> m g
    -> m g
withScope n = local $ over _2 ((n ^. PLC.unique . coerced, n ^. PLC.str) :)


bindingDeps
    :: (DepGraph g, MonadReader Ctx m, PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
    => Binding tyname name a
    -> m g
bindingDeps b = case b of
    TermBind _ _ d@(VarDecl _ n _) rhs -> do
        vDeps <- varDeclDeps d
        tDeps <- withLet n $ termDeps rhs
        pure $ G.overlay vDeps tDeps
    TypeBind _ d@(TyVarDecl _ n _) rhs -> do
        vDeps <- tyVarDeclDeps d
        tDeps <- withLet n $ typeDeps rhs
        pure $ G.overlay vDeps tDeps
    DatatypeBind _ (Datatype _ d tvs destr constrs) -> do
        vDeps <- tyVarDeclDeps d
        tvDeps <- traverse tyVarDeclDeps tvs
        cstrDeps <- traverse varDeclDeps constrs
        -- All the datatype bindings depend on each other since they can't be used separately. Consider
        -- the identity function on a datatype type - it only uses the type variable, but the whole definition
        -- will therefore be kept, and so we must consider any uses in e.g. the constructors as live.
        let tyus = fmap (\n -> (n ^. PLC.unique . coerced, "mpla")) $ tyVarDeclName d : fmap tyVarDeclName tvs
        let tus = fmap (\n -> (n ^. PLC.unique . coerced, "mpla")) $ destr : fmap varDeclName constrs
        let localDeps = G.clique (fmap LetOrFree $ tyus ++ tus)
        pure $ G.overlays $ [vDeps] ++ tvDeps ++ cstrDeps ++ [localDeps]

varDeclDeps
    :: (DepGraph g, MonadReader Ctx m, PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
    => VarDecl tyname name a
    -> m g
varDeclDeps (VarDecl _ n ty) = withLet n $ typeDeps ty

-- Here for completeness, but doesn't do much
tyVarDeclDeps
    :: (G.Graph g, MonadReader Ctx m)
    => TyVarDecl tyname a
    -> m g
tyVarDeclDeps _ = pure G.empty

-- | Compute the dependency graph of a term. Takes an initial 'Node' indicating what the term itself depends on
-- (usually 'Root' if it is the real term you are interested in).
termDeps
    :: (DepGraph g, MonadReader Ctx m, PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
    => Term tyname name a
    -> m g
termDeps = \case
    LamAbs _ n ty t -> do
      newScopeEdge <- G.edge (SmallOrBigLambda (n ^. PLC.unique . coerced, n ^. PLC.str)) . (\case (v:_) -> SmallOrBigLambda v; _ -> Top) . snd <$> ask
      typGraph <- withScope n $ typeDeps ty
      tGraph <- withScope n $ termDeps t
      pure $ G.overlays [newScopeEdge,typGraph,tGraph]
    TyAbs _ n _ki t -> do
      newScopeEdge <- G.edge (SmallOrBigLambda (n ^. PLC.unique . coerced, n ^. PLC.str)) . (\case (v:_) -> SmallOrBigLambda v; _ -> Top) . snd <$> ask
      bodyGraph <- withScope n $ termDeps t
      pure $ newScopeEdge `G.overlay` bodyGraph
    Let _ _ bs t -> do
        bGraphs <- traverse bindingDeps bs
        bodyGraph <- termDeps t
        pure $ G.overlays $ bGraphs ++ [bodyGraph]
    Var _ n -> recordDeps [(n ^. PLC.unique . coerced, n ^. PLC.str)]
    x -> do
        tds <- traverse termDeps (x ^.. termSubterms)
        tyds <- traverse typeDeps (x ^.. termSubtypes)
        pure $ G.overlays $ tds ++ tyds

-- | Compute the dependency graph of a type. Takes an initial 'Node' indicating what the type itself depends on
-- (usually 'Root' if it is the real type you are interested in).
typeDeps
    :: (DepGraph g, MonadReader Ctx m, PLC.HasUnique (tyname a) PLC.TypeUnique)
    => Type tyname a
    -> m g
typeDeps ty =
    -- The dependency graph of a type is very simple since it doesn't have any internal let-bindings. So we just
    -- need to find all the used variables and mark them as dependencies of the current node.
    let used = Usages.allUsed $ Usages.runTypeUsages ty
    in recordDeps (Set.toList used)



-- | Implementation taken from: https://github.com/snowleopard/alga/issues/165
--
-- It uses the 'AM.transitiveClosure' to compute the transitive reduction.
-- Note: there may be a faster implementation by using 'AM.dfsForest'
transitiveReduction :: Ord a => AM.AdjacencyMap a -> Maybe (AM.AdjacencyMap a)
transitiveReduction g
    | not (AM.isAcyclic g) = Nothing
    | otherwise         = Just $ AM.vertices vs `AM.overlay` AM.edges es
  where
    vs = AM.vertexList g
    es = filter (not . transitive) (AM.edgeList g)
    jumpGraph         = let t = AM.transitiveClosure g in AM.compose t t
    transitive (x, y) = AM.hasEdge x y jumpGraph


cleanAndCollectLets :: (PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
                    => Term tyname name a
                    -> (Term tyname name a
                       ,M.Map PLC.Unique (a, Recursivity, Binding tyname name a)
                       )
cleanAndCollectLets = flip runState M.empty . cleanAndCollectLets'
  where
    cleanAndCollectLets' = \case
      Let a r bs t' -> do

        bs' <- traverse (bindingSubterms cleanAndCollectLets') bs

        let newMap = M.fromList $
              fmap (\ b -> (bindName b, (a,r,b)))
              bs'
        modify (M.union newMap)

        cleanAndCollectLets' t'

      t -> termSubterms cleanAndCollectLets' t

    -- TODO: maybe remove the boilerplate by having a lens "name" for a Binding
    bindName = \case TermBind _ _ (VarDecl _ n _) _ -> n ^. PLC.unique . coerced
                     TypeBind _ (TyVarDecl _ n _) rhs -> n ^. PLC.unique . coerced
                     DatatypeBind _ (Datatype _ d tvs destr constrs) -> error "TODO: not implemented yet"
