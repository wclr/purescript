-- | Provides the ability to sort modules based on module dependencies
module Language.PureScript.ModuleDependencies
  ( DependencyDepth(..)
  , sortModules
  , ModuleGraph
  , ModuleSignature(..)
  , moduleSignature
  ) where

import Protolude hiding (head, trace)

import Data.Array ((!))
import Data.Graph (SCC(..), graphFromEdges, reachable, stronglyConnComp)
import Data.Set qualified as S
import Data.Map qualified as M
import Language.PureScript.AST (Declaration(..), ErrorMessageHint(..), Module(..), SourceSpan)
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Crash (internalError)
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage(..), addHint, errorMessage', errorMessage'', parU)
import Language.PureScript.Names (ModuleName)
import Language.PureScript.AST.Declarations (DeclarationRef(..))
import Debug.Trace (trace)

-- | A list of modules with their transitive dependencies
type ModuleGraph = [(ModuleName, [ModuleName])]

-- | A module signature for sorting dependencies.
data ModuleSignature = ModuleSignature
  { sigSourceSpan :: SourceSpan
  , sigModuleName :: ModuleName
  , sigImports :: [(ModuleName, SourceSpan)]
  , sigReExports :: S.Set ModuleName
  }

data DependencyDepth = Direct | ReExports | Transitive

-- | Sort a collection of modules based on module dependencies.
--
-- Reports an error if the module graph contains a cycle.
sortModules
  :: forall m a
   . MonadError MultipleErrors m
  => DependencyDepth
  -> (a -> ModuleSignature)
  -> [a]
  -> m ([a], ModuleGraph)
sortModules dependencyDepth toSig ms = do
    let
      ms' = (\m -> (m, toSig m)) <$> ms
      mns = S.fromList $ map (sigModuleName . snd) ms'
    verts <- parU ms' (toGraphNode mns)
    ms'' <- parU (stronglyConnComp verts) toModule
    let (graph, fromVertex, toVertex) = graphFromEdges verts
        moduleGraph = do (_, mn, _) <- verts
                         let v       = fromMaybe (internalError "sortModules: vertex not found") (toVertex mn)
                             deps    = case dependencyDepth of
                                         Direct -> graph ! v
                                         Transitive -> reachable graph v
                             toKey i = case fromVertex i of (_, key, _) -> key
                         return (mn, filter (/= mn) (map toKey deps))
    return (fst <$> ms'', moduleGraph)
  where
    toGraphNode :: S.Set ModuleName -> (a, ModuleSignature) -> m ((a, ModuleSignature), ModuleName, [ModuleName])
    toGraphNode mns m@(_, ModuleSignature _ mn deps _) = do
      void . parU deps $ \(dep, pos) ->
        when (dep `notElem` C.primModules && S.notMember dep mns) .
          throwError
            . addHint (ErrorInModule mn)
            . errorMessage' pos
            $ ModuleNotFound dep
      pure (m, mn, map fst deps)


sortModules_ ::
  forall m a.
  MonadError MultipleErrors m =>
  DependencyDepth ->
  (a -> ModuleSignature) ->
  [a] ->
  m ([a], ModuleGraph)
sortModules_ depth toSig ms = do
  let
    ms' = (\m -> (m, toSig m)) <$> ms
    mns = S.fromList $ map (sigModuleName . snd) ms'
  -- this is crap
  verts <- parU ms' (toGraphNode True mns)
  verts' <- parU ms' (toGraphNode False mns)
  ms'' <- parU (stronglyConnComp verts') toModule
  let (graph, fromVertex, toVertex) = graphFromEdges verts
      moduleGraph = do
        ((_, ModuleSignature _ _ directs _), mn, _) <- verts
        let v = fromMaybe (internalError "sortModules: vertex not found") . toVertex
            deps = case depth of
              Direct -> graph ! v mn
              Transitive -> reachable graph (v mn)
              ReExports -> S.toList $ foldMap (S.fromList . reachable graph . v)
                (filter (flip notElem C.primModules) (fst <$> directs))
            toKey i = case fromVertex i of (_, key, _) -> key
        return (mn, filter (/= mn) (map toKey deps))
  return (fst <$> ms'', moduleGraph)
  where
    toGraphNode :: Bool -> S.Set ModuleName -> (a, ModuleSignature) -> m ((a, ModuleSignature), ModuleName, [ModuleName])
    toGraphNode useCutOff mns m@(_, ModuleSignature _ mn deps reexports) = do
      void . parU deps $ \(dep, pos) ->
        when (dep `notElem` C.primModules && S.notMember dep mns)
          . throwError
          . addHint (ErrorInModule mn)
          . errorMessage' pos
          $ ModuleNotFound dep
      let cutoff = case depth of
            ReExports | useCutOff-> filter (flip S.member reexports)
            _ -> identity
      pure (m, mn, cutoff $ map fst deps)


-- | Calculate a list of used modules based on explicit imports and qualified names.
usedModules :: Declaration -> Maybe (ModuleName, SourceSpan)
-- Regardless of whether an imported module is qualified we still need to
-- take into account its import to build an accurate list of dependencies.
usedModules (ImportDeclaration (ss, _) mn _ _) = pure (mn, ss)
usedModules _ = Nothing

-- | Convert a strongly connected component of the module graph to a module
toModule :: MonadError MultipleErrors m => SCC (a, ModuleSignature) -> m (a, ModuleSignature)
toModule (AcyclicSCC m) = return m
toModule (CyclicSCC ms) =
  case nonEmpty ms of
    Nothing ->
      internalError "toModule: empty CyclicSCC"
    Just ms' ->
      throwError
        . errorMessage'' (fmap (sigSourceSpan . snd) ms')
        $ CycleInModules (map (sigModuleName . snd) ms')

moduleSignature :: Module -> ModuleSignature
moduleSignature m@(Module ss _ mn ds _) =
  ModuleSignature ss mn (ordNub (mapMaybe usedModules ds)) (moduleReExports m)

moduleReExports :: Module -> S.Set ModuleName
moduleReExports (Module _ _ _ ds (Just refs)) =
  foldl (flip go) S.empty (trace "moduleReExports" refs)
  where
    refAs (ImportDeclaration _ mn _ Nothing) = Just (mn, mn)
    refAs (ImportDeclaration _ mn _ (Just as)) = Just (as, mn)
    refAs _ = Nothing
    refMap = M.fromList (mapMaybe refAs ds)
    go (ModuleRef _ mn) = maybe identity S.insert (M.lookup mn refMap)
    go _ = identity
moduleReExports _ = S.empty
