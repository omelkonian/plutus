{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Functions for computing the dependency graph of variables within a term or type. A "dependency" between
-- two nodes "A depends on B" means that B cannot be removed from the program without also removing A.
module Language.PlutusIR.Analysis.FreeGraph2 (Node (..), DepGraph, runTermDeps, runTypeDeps) where

import           Language.PlutusIR

import           Control.Lens
import           Control.Monad.Reader
import Control.Monad.RWS
import           Data.Text                          (Text, unpack)

import Data.Foldable (traverse_)
import qualified Language.PlutusCore                as PLC
import qualified Language.PlutusCore.Name           as PLC
import qualified Language.PlutusIR.Analysis.Usages  as Usages

import qualified Algebra.Graph.Class                as G
import qualified Data.Set                           as Set
import Data.List (find,intersect, (\\))

import Language.PlutusIR.Parser
import Language.PlutusIR.Compiler.Provenance
import           Language.PlutusIR.Transform.Rename          ()
import           Language.PlutusCore.Quote
import Language.PlutusCore.Pretty
import qualified Language.PlutusIR.Analysis.Dependencies as D

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AM (reachable)

-- | A node in a dependency graph. Either a specific 'PLC.Unique', or a specific
-- node indicating the root of the graph. We need the root node because when computing the
-- dependency graph of, say, a term, there will not be a binding for the term itself which
-- we can use to represent it in the graph.
data Node = LetNode {u :: (PLC.Unique,Text)}
          | LamNode {u :: (PLC.Unique,Text)}
          deriving (Eq, Ord)

instance Show Node where
  show = \case
    LamNode v -> "la" ++ show (fst v) ++ unpack (snd v)
    LetNode v -> "le" ++ show (fst v) ++ unpack (snd v)


-- | A constraint requiring @g@ to be a 'G.Graph' (so we can compute e.g. a @Relation@ from it), whose
-- vertices are 'Node's.
type DepGraph g = (G.Graph g, g~AM.AdjacencyMap Node, (G.Vertex g)~Node)

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
runTermDeps t = fst $ execRWS (termDeps t :: DepGraph g => RWS Scope () g ()) [] G.empty

-- the reader context enhanced by a full enclosing lambda scope
type Ctx = (         -- points to the let of the rhs term we are currently in, nothing if not in an rhs but at the root
           Scope -- letsRhsInScope
           )

-- invariant: left-to-right scoping order, i.e. head is scoped by the tail
type Scope = [Node]

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
runTypeDeps t = fst $ execRWS (typeDeps t :: DepGraph g => RWS Scope () g ()) [] G.empty


-- | Record some dependencies on the current node.
recordDeps
    :: (DepGraph g, MonadReader Ctx m, MonadState g m)
    => [(PLC.Unique,Text)]
    -> m ()
recordDeps deps = do
    currentScope <- ask
    modify (\ g ->
              G.overlays $ g : fmap (\ dep -> let
                                (left, right) = span (\ n -> u n /= dep) currentScope
                                (lamsBeforeNearestLet, mnearestLet) = span (\case LamNode _ -> True; _ -> False) left
                                --leftLetDependants = filter (\case LetNode _ -> True; _ -> False) left -- take all lets introduced after this dependency
                                in case right of
                                  found@(LamNode _) : _ ->
                                       case mnearestLet of
                                         nearestLet : _ -> G.connect (G.vertex nearestLet) (G.vertex found) -- connect the nearestLet that is introduced before this lambda
                                         _ -> G.empty -- ignore the dep, if the nearestLet occurs after the lambda.
                                  found@(LetNode _) : _ ->
                                       case mnearestLet of
                                         nearestLet : _ | nearestLet /= found -> G.connect (G.vertex nearestLet) (G.vertex found) -- avoid adding a dependency to self
                                         _ -> G.empty
                                  [] -> -- this occurence not found, means is is a Let-bound variable inside a let-body-in section. We have to check the state
                                       case mnearestLet of
                                         nearestLet : _  -> let depDeps = AM.reachable (LetNode dep) g \\ [LetNode dep] -- exlude self from the deps
                                                            in case depDeps `Data.List.intersect` (nearestLet : lamsBeforeNearestLet) of
                                                                 [] -> G.connect (G.vertex nearestLet) (G.vertex $ LetNode dep)
                                                                 _ -> G.empty
                                         _ -> G.empty

                                      -- (scopedUpToLambdaDependants,_) = span (\case LetNode _ -> True; _ -> False) scoped -- take all in-scope let-rhses until reaching a lambda
                                    --    allLetDependants = filter (\case LetNode _ -> True; _ -> False) currentScope
                                    -- in G.connect (G.vertices $ allLetDependants) (G.vertex $ LetNode dep)




                             -- let freeLetDependants = filter (\case LetNode _ -> True; _ -> False) free -- take all let-rhses introduced before until reaching a lambda
                                                              -- in G.connect (G.vertices $ scopedLetDependants -- all let-rhses introduced after this let
                                                              --                          ++ freeLetDependants -- the let-rhses introduced before this let until hitting a lambda
                                                              --              ) (G.vertex found)
                             ) deps)

    -- let (freeLetDependants,_) = span (\case LetNode _ -> True; _ -> False) free -- take all let-rhses introduced before until reaching a lambda
    -- pure $ maybe G.empty
    --       (\currentLet -> G.connect (G.vertices [LetNode currentLet]) (G.vertices (mapMaybe (\ v ->
    --                                                                                              find (\ n -> (u n) == v) currentScope) 
    --                                                                                 us)))
    --       mcurrentLet
-- | Process the given action with the given name as the current node.
enterLetRhs
    :: (MonadReader Ctx m, PLC.HasUnique n u)
    => n
    -> m g
    -> m g
enterLetRhs n = local (LetNode (n ^. PLC.unique . coerced, n ^. PLC.str) :)

enterLam n = local (n :)

bindingDeps
    :: (DepGraph g, MonadReader Ctx m, MonadState g m, PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
    => Binding tyname name a
    -> m ()
bindingDeps b = case b of
    TermBind _ _ d@(VarDecl _ n _) rhs -> do
        -- TODO: move this modify to enterLetRhs
        modify (G.overlay (G.vertex $ LetNode (n ^. PLC.unique . coerced, n ^. PLC.str))) -- introduced the let-identifier to the graph
        varDeclDeps d
        enterLetRhs n $ termDeps rhs
    TypeBind _ d@(TyVarDecl _ n _) rhs -> do
        -- TODO: move this modify to enterLetRhs
        modify (G.overlay (G.vertex $ LetNode (n ^. PLC.unique . coerced, n ^. PLC.str))) -- introduced the let-identifier to the graph
        tyVarDeclDeps d
        enterLetRhs n $ typeDeps rhs
    DatatypeBind _ (Datatype _ d tvs destr constrs) -> do
        tyVarDeclDeps d
        traverse_ tyVarDeclDeps tvs
        traverse_ varDeclDeps constrs
        -- All the datatype bindings depend on each other since they can't be used separately. Consider
        -- the identity function on a datatype type - it only uses the type variable, but the whole definition
        -- will therefore be kept, and so we must consider any uses in e.g. the constructors as live.
        let tyus = fmap (\n -> (n ^. PLC.unique . coerced, "mpla")) $ tyVarDeclName d : fmap tyVarDeclName tvs
        let tus = fmap (\n -> (n ^. PLC.unique . coerced, "mpla")) $ destr : fmap varDeclName constrs
        let localDeps = G.clique (fmap LetNode $ tyus ++ tus)
        modify (G.overlay localDeps)

varDeclDeps
    :: (DepGraph g, MonadReader Ctx m, MonadState g m, PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
    => VarDecl tyname name a
    -> m ()
varDeclDeps (VarDecl _ n ty) = enterLetRhs n $ typeDeps ty

-- Here for completeness, but doesn't do much
tyVarDeclDeps
    :: (G.Graph g, MonadReader Ctx m, MonadState g m)
    => TyVarDecl tyname a
    -> m ()
tyVarDeclDeps _ = pure () -- G.empty


addLamToGraph :: (DepGraph g, MonadReader Ctx m, MonadState g m) => Node -> m ()
addLamToGraph n =  do
  closestLam <- asks (find (\case LamNode _ -> True; _ -> False))
  modify (case closestLam of
    Nothing -> G.overlay (G.vertex n)
    Just lam -> G.overlay (G.edge n lam))

-- | Compute the dependency graph of a term. Takes an initial 'Node' indicating what the term itself depends on
-- (usually 'Root' if it is the real term you are interested in).
termDeps
    :: (DepGraph g, MonadReader Ctx m, MonadState g m, PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
    => Term tyname name a
    -> m ()
termDeps = \case
    LamAbs _ n ty t -> do
      let newLam = LamNode (n ^. PLC.unique . coerced, n ^. PLC.str)
      addLamToGraph newLam
      enterLam newLam $ typeDeps ty
      enterLam newLam $ termDeps t
    TyAbs _ n _ki t -> do
      let newLam = LamNode (n ^. PLC.unique . coerced, n ^. PLC.str)
      addLamToGraph newLam
      enterLam newLam $ termDeps t
    Let _ _ bs t -> do
        traverse_ bindingDeps bs
        -- bodyGraph <- 
          -- addToScope (fmap (LetNode . bindingName) bs) $ 
        termDeps t
    Var _ n -> recordDeps [(n ^. PLC.unique . coerced, n ^. PLC.str)]
    x -> do
        traverse_ termDeps (x ^.. termSubterms)
        traverse_ typeDeps (x ^.. termSubtypes)

-- | Compute the dependency graph of a type. Takes an initial 'Node' indicating what the type itself depends on
-- (usually 'Root' if it is the real type you are interested in).
typeDeps
    :: (DepGraph g, MonadReader Ctx m, MonadState g m, PLC.HasUnique (tyname a) PLC.TypeUnique)
    => Type tyname a
    -> m ()
typeDeps ty =
    -- The dependency graph of a type is very simple since it doesn't have any internal let-bindings. So we just
    -- need to find all the used variables and mark them as dependencies of the current node.
    let used = Usages.allUsed $ Usages.runTypeUsages ty
    in recordDeps (Set.toList used)

-- Helper
addToScope s = local (s++)

bindingName :: (PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
            => Binding tyname name a -> (PLC.Unique, Text)
bindingName = \case
    TermBind _ _ (VarDecl _ n _) _ -> (n ^. PLC.unique . coerced, n ^. PLC.str)
    TypeBind _ (TyVarDecl _ n _) _ -> (n ^. PLC.unique . coerced, n ^. PLC.str)
    -- FIXME: DatatypeBind x d -> DatatypeBind x <$> datatypeSubtypes f d

