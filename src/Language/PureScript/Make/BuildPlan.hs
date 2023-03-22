module Language.PureScript.Make.BuildPlan
  ( BuildPlan(bpEnv, bpIndex)
  , BuildJobResult(..)
  , construct
  , getResult
  , getPrevResult
  , collectResults
  , markComplete
  , needsRebuild
  ) where

import Prelude

import Control.Concurrent.Async.Lifted as A
import Control.Concurrent.Lifted as C
import Control.Monad.Base (liftBase)
import Control.Monad (foldM)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Foldable (foldl')
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe, isNothing, isJust)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Language.PureScript.AST (Module, getModuleName)
import Language.PureScript.Crash (internalError)
import Language.PureScript.CST qualified as CST
import Language.PureScript.Errors (MultipleErrors(..))
import Language.PureScript.Externs (ExternsFile)
import Language.PureScript.Make.Actions as Actions
import Language.PureScript.Make.Cache (CacheDb, CacheInfo, checkChanged)
import Language.PureScript.Make.ExternsDiff (ExternsDiff, getExternsDiff)
import Language.PureScript.Names (ModuleName)
import Language.PureScript.Sugar.Names.Env (Env, primEnv)
import System.Directory (getCurrentDirectory)

import Debug.Trace (trace)
import Control.Exception.Lifted (evaluate)

-- | The BuildPlan tracks information about our build progress, and holds all
-- prebuilt modules for incremental builds.
data BuildPlan = BuildPlan
  { bpPrebuilt :: M.Map ModuleName Prebuilt
  , bpPreviousBuilt :: M.Map ModuleName (Bool, Prebuilt)
  , bpBuildJobs :: M.Map ModuleName BuildJob
  , bpEnv :: C.MVar Env
  , bpIndex :: C.MVar Int
  }

data Prebuilt = Prebuilt
  { pbExternsFile :: ExternsFile
  }

newtype BuildJob = BuildJob
  { bjResult :: C.MVar BuildJobResult
    -- ^ Note: an empty MVar indicates that the build job has not yet finished.
  }

data BuildJobResult
  = BuildJobSucceeded !MultipleErrors !ExternsFile
  -- ^ Succeeded, with warnings and externs
  --
  | BuildJobFailed !MultipleErrors
  -- ^ Failed, with errors

  | BuildJobSkipped
  -- ^ The build job was not run, because an upstream build job failed

buildJobSuccess :: BuildJobResult -> Maybe (MultipleErrors, ExternsFile)
buildJobSuccess (BuildJobSucceeded warnings externs) = Just (warnings, externs)
buildJobSuccess _ = Nothing

-- | Information obtained about a particular module while constructing a build
-- plan; used to decide whether a module needs rebuilding.
data RebuildStatus = RebuildStatus
  { statusModuleName :: ModuleName
  , statusRebuildNever :: Bool
  , statusNewCacheInfo :: Maybe CacheInfo
    -- ^ New cache info for this module which should be stored for subsequent
    -- incremental builds. A value of Nothing indicates that cache info for
    -- this module should not be stored in the build cache, because it is being
    -- rebuilt according to a RebuildPolicy instead.
  , statusPrebuilt :: Maybe UTCTime
    -- ^ Prebuilt timestamp (compilation time) for this module.
  , statusUpToDate :: Bool
  -- ^ Whether or not module timestamp/content changed, checked against provided
  -- cache-db.
  }

noBarrierErr :: T.Text -> a
noBarrierErr infx = internalError $ "make: " <> T.unpack infx <> " no barrier"

-- | Called when we finished compiling a module and want to report back the
-- compilation result, as well as any potential errors that were thrown.
markComplete
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> ModuleName
  -> BuildJobResult
  -> m ()
markComplete buildPlan moduleName result = do
  let BuildJob rVar = fromMaybe (noBarrierErr "markComplete") $ M.lookup moduleName (bpBuildJobs buildPlan)
  putMVar rVar result

-- | Whether or not the module with the given ModuleName needs to be rebuilt
needsRebuild :: BuildPlan -> ModuleName -> Bool
needsRebuild bp moduleName = M.member moduleName (bpBuildJobs bp)

-- | Collects results for all prebuilt as well as rebuilt modules. This will
-- block until all build jobs are finished. Prebuilt modules always return no
-- warnings.
collectResults
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> m (M.Map ModuleName BuildJobResult)
collectResults buildPlan = do
  let prebuiltResults = M.map (BuildJobSucceeded (MultipleErrors []) . pbExternsFile) (bpPrebuilt buildPlan)
  barrierResults <- traverse (readMVar . bjResult) $ bpBuildJobs buildPlan
  pure (M.union prebuiltResults barrierResults)

-- | Gets the the build result for a given module name independent of whether it
-- was rebuilt or prebuilt. Prebuilt modules always return no warnings.
getResult
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> ModuleName
  -> m (Maybe (MultipleErrors, (ExternsFile, ExternsDiff)))
getResult buildPlan moduleName =
  -- may bring back first lookup for bpPrebuilt
  case M.lookup moduleName (bpBuildJobs buildPlan) of
    Just bj -> do
      r <- readMVar $ bjResult bj
      let prevExts =
            pbExternsFile <$> snd <$> M.lookup moduleName (bpPreviousBuilt buildPlan)
      --evaluate $ trace ("prevExts" <> show moduleName <> show prevExts) ()
      --pure $ (\(w, ex) -> (w, (ex, getExternsDiff ex prevExts))) <$> buildJobSuccess r
      pure $ fmap ((,) <*> flip getExternsDiff prevExts) <$> buildJobSuccess r
    Nothing -> do
      let exts = pbExternsFile
            $ fromMaybe (noBarrierErr "getResult")
            $ M.lookup moduleName (bpPrebuilt buildPlan)
      pure (Just (MultipleErrors [], (exts, getExternsDiff exts Nothing )))

  -- case M.lookup moduleName (bpPrebuilt buildPlan) of
  --   Just es -> do
  --     let exts = pbExternsFile es
  --     pure (Just (MultipleErrors [], (exts, getExternsDiff exts Nothing ))
  --   Nothing -> do
  --     r <- readMVar $ bjResult $ fromMaybe (internalError "make: no barrier")
  --       $ M.lookup moduleName (bpBuildJobs buildPlan)
  --     pure $ buildJobSuccess r

-- | Gets preloaded built result for modules that are going to be built . This
-- result is used to skip rebuilding if deps (their externs) have not change.
--
-- We only return previous built result for up to date modules, because changed
-- modules should be rebuilt anyway.
getPrevResult :: BuildPlan -> ModuleName -> Maybe ExternsFile
getPrevResult buildPlan moduleName =
  case M.lookup moduleName (bpPreviousBuilt buildPlan) of
    Just (True, prebuilt) -> Just (pbExternsFile prebuilt)
    _ -> Nothing

-- | Constructs a BuildPlan for the given module graph.
--
-- The given MakeActions are used to collect various timestamps in order to
-- determine whether a module needs rebuilding.
construct
  :: forall m. MonadBaseControl IO m
  => MakeActions m
  -> CacheDb
  -> ([CST.PartialResult Module], [(ModuleName, [ModuleName])])
  -> Bool
  -- ^ If True will preload all the externs, otherwise will load only needed for
  -- the build.
  -> m (BuildPlan, CacheDb)
construct MakeActions{..} cacheDb (sorted, graph) preloadAll = do
  let sortedModuleNames = map (getModuleName . CST.resPartial) sorted
  rebuildStatuses <- A.forConcurrently sortedModuleNames getRebuildStatus

  -- find modules with prebuilt results that is valid to be used
  let allPrebuilt =
        foldl' collectPrebuiltModules M.empty $
          mapMaybe (\s -> (statusModuleName s, statusRebuildNever s,) <$> statusPrebuilt s)
          (filter statusUpToDate rebuildStatuses)

  -- let statuses = map (\s -> (statusModuleName s, statusPrebuilt s)) rebuildStatuses
  -- evaluate $ trace ("rebuildStatuses: " <> show statuses) ()

  -- other modules have to be built
  let toBeRebuilt = filter (not . flip M.member allPrebuilt) sortedModuleNames

  -- we need only prebuilt results for deps of the modules to be build
  let toBeRebuiltSet = S.fromList toBeRebuilt
  let depsFolder m dep = case M.lookup dep allPrebuilt of
        Just _
          | not (S.member dep toBeRebuiltSet) ->
            M.insert dep dep m
        _ -> m

  let toLoadPrebuilt =
        if preloadAll
          then M.mapWithKey const allPrebuilt
          else foldl' ((. getDeps) . foldl' depsFolder) M.empty toBeRebuilt

  -- we may need previously built results for modules to be build
  -- to skip rebuilding if deps have not changed
  let toLoadPrevFilter (RebuildStatus {statusModuleName = mn, ..}) =
        if isJust statusPrebuilt && S.member mn toBeRebuiltSet then
          Just (mn, (statusUpToDate, mn))
        else
          Nothing
  let toLoadPrev = M.fromList $ mapMaybe toLoadPrevFilter rebuildStatuses

  (prebuiltLoad, prevLoad) <-
    A.concurrently
      (A.forConcurrently toLoadPrebuilt loadPrebuilt)
      (A.forConcurrently toLoadPrev (\(up, mn) -> fmap (up, ) <$> loadPrebuilt mn ))

  let prebuilt = M.mapMaybe id prebuiltLoad
  let previous = M.mapMaybe id prevLoad

  -- evaluate $ trace ("toBeRebuilt" <> show toBeRebuilt) ()
  -- evaluate $ trace ("prebuilt:" <> show (M.keys prebuilt)) ()
  -- evaluate $ trace ("previous:" <> show (M.keys previous)) ()

  -- if for some reason (wrong version, files corruption) loading externs failed,
  -- those modules should be rebuilt too
  let failedLoads = M.keys $ M.filter isNothing prebuiltLoad
  buildJobs <- foldM makeBuildJob M.empty (toBeRebuilt <> failedLoads)

  env <- C.newMVar primEnv
  idx <- C.newMVar 1
  pure
    ( BuildPlan prebuilt previous buildJobs env idx
    , let
        update = flip $ \s ->
          M.alter (const (statusNewCacheInfo s)) (statusModuleName s)
      in
        foldl' update cacheDb rebuildStatuses
    )
  where
    loadPrebuilt = fmap (fmap Prebuilt . snd) . readExterns

    makeBuildJob prev moduleName = do
      buildJob <- BuildJob <$> C.newEmptyMVar
      pure (M.insert moduleName buildJob prev)

    getRebuildStatus :: ModuleName -> m RebuildStatus
    getRebuildStatus moduleName = do
      inputInfo <- getInputTimestampsAndHashes moduleName
      case inputInfo of
        Left RebuildNever -> do
          timestamp <- getOutputTimestamp moduleName
          pure (RebuildStatus
            { statusModuleName = moduleName
            , statusRebuildNever = True
            , statusPrebuilt = timestamp
            , statusUpToDate = True
            , statusNewCacheInfo = Nothing
            })
        Left RebuildAlways -> do
          pure (RebuildStatus
            { statusModuleName = moduleName
            , statusRebuildNever = False
            , statusPrebuilt = Nothing
            , statusUpToDate = False
            , statusNewCacheInfo = Nothing
            })
        Right cacheInfo -> do
          cwd <- liftBase getCurrentDirectory
          (newCacheInfo, isUpToDate) <- checkChanged cacheDb moduleName cwd cacheInfo
          timestamp <- getOutputTimestamp moduleName

          pure (RebuildStatus
            { statusModuleName = moduleName
            , statusRebuildNever = False
            , statusPrebuilt = timestamp
            , statusUpToDate = isUpToDate
            , statusNewCacheInfo = Just newCacheInfo
            })

    getDeps = fromMaybe (internalError "make: module not found in dependency graph.") . flip lookup graph

    collectPrebuiltModules :: M.Map ModuleName UTCTime -> (ModuleName, Bool, UTCTime) -> M.Map ModuleName UTCTime
    collectPrebuiltModules prev (moduleName, rebuildNever, pb)
      | rebuildNever = M.insert moduleName pb prev
      | otherwise = do
          let deps = getDeps moduleName
          case traverse (flip M.lookup prev) deps of
            Nothing ->
              -- If we end up here, one of the dependencies didn't exist in the
              -- prebuilt map and so we know a dependency needs to be rebuilt, which
              -- means we need to be rebuilt in turn.
              prev
            Just modTimes ->
              case maximumMaybe modTimes of
                Just depModTime | pb < depModTime ->
                  prev
                _ -> M.insert moduleName pb prev

maximumMaybe :: Ord a => [a] -> Maybe a
maximumMaybe [] = Nothing
maximumMaybe xs = Just $ maximum xs
