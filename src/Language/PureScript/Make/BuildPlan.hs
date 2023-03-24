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

import Control.Concurrent.Async.Lifted qualified as A
import Control.Concurrent.Lifted qualified as C
import Control.Monad.Base (liftBase)
import Control.Monad (foldM)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.Foldable (foldl')
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe, isNothing)
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
import Language.PureScript.Make.ExternsDiff (ExternsDiff, diffExterns, emptyDiff)
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
  = BuildJobSucceeded !MultipleErrors !ExternsFile (Maybe ExternsDiff)
  -- ^ Succeeded, with warnings and externs, also holds externs diff with
  -- previous build result if any (lazily evaluated).
  --
  | BuildJobFailed !MultipleErrors
  -- ^ Failed, with errors.

  | BuildJobSkipped
  -- ^ The build job was not run, because an upstream build job failed.

type SuccessResult = (MultipleErrors, (ExternsFile, Maybe ExternsDiff))

buildJobSuccess :: BuildJobResult -> Maybe SuccessResult
buildJobSuccess (BuildJobSucceeded warnings externs diff) = Just (warnings, (externs, diff))
buildJobSuccess _ = Nothing

-- | Information obtained about a particular module while constructing a build
-- plan; used to decide whether a module needs rebuilding.
data RebuildStatus = RebuildStatus
  { rsModuleName :: ModuleName
  , rsRebuildNever :: Bool
  , rsNewCacheInfo :: Maybe CacheInfo
    -- ^ New cache info for this module which should be stored for subsequent
    -- incremental builds. A value of Nothing indicates that cache info for
    -- this module should not be stored in the build cache, because it is being
    -- rebuilt according to a RebuildPolicy instead.
  , rsPrebuilt :: Maybe UTCTime
    -- ^ Prebuilt timestamp (compilation time) for this module.
  , rsUpToDate :: Bool
    -- ^ Whether or not module (timestamp or content) changed since previous
    -- compilation (checked against provided cache-db info).
  }

-- | Construct common error message indicating a bug in the internal logic
barrierError :: T.Text -> a
barrierError infx = internalError $ "make: " <> T.unpack infx <> " no barrier"

-- | Get externs diff is for constructing BuildJobSucceeded.
getPrevDiff :: BuildPlan -> ModuleName -> ExternsFile -> Maybe ExternsDiff
getPrevDiff buildPlan moduleName exts =
  --trace ("getPrevDiff:" <> show moduleName) $
  diffExterns exts <$> prevExts
  where
    prevExts = --trace ("getPrevDiff prevExts:" <> show moduleName) $
      pbExternsFile <$> snd <$> M.lookup moduleName (bpPreviousBuilt buildPlan)

-- | Called when we finished compiling a module and want to report back the
-- compilation result, as well as any potential errors that were thrown.
markComplete
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> ModuleName
  -> BuildJobResult
  -> m ()
markComplete buildPlan moduleName result = do
  let BuildJob rVar =
        fromMaybe (barrierError "markComplete") $ M.lookup moduleName (bpBuildJobs buildPlan)
  -- add externs diff to success result
  case buildJobSuccess result of
    Just (errs, (exts, _)) ->
      let diff = getPrevDiff buildPlan moduleName exts
      in C.putMVar rVar $ BuildJobSucceeded errs exts diff
    _ ->
      C.putMVar rVar result

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
  let mapExts exts = BuildJobSucceeded (MultipleErrors []) exts Nothing
  let prebuiltResults =
        M.map (mapExts . pbExternsFile) (bpPrebuilt buildPlan)
  barrierResults <- traverse (C.readMVar . bjResult) $ bpBuildJobs buildPlan
  pure (M.union prebuiltResults barrierResults)

-- | Gets the the build result for a given module name independent of whether it
-- was rebuilt or prebuilt. Prebuilt modules always return no warnings.
getResult
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> ModuleName
  -> m (Maybe SuccessResult)
getResult buildPlan moduleName =
  -- may bring back first lookup for bpPrebuilt
  case M.lookup moduleName (bpBuildJobs buildPlan) of
    Just bj ->
      buildJobSuccess <$> C.readMVar (bjResult bj)
    Nothing -> do
      let exts = pbExternsFile
            $ fromMaybe (barrierError "getResult")
            $ M.lookup moduleName (bpPrebuilt buildPlan)
      pure (Just (MultipleErrors [], (exts, Just emptyDiff )))

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

  -- find modules with prebuilt results that don't need to be rebuilt
  let allPrebuilt =
        foldl' collectPrebuiltModules M.empty $
          mapMaybe (\s -> (rsModuleName s, rsRebuildNever s,) <$> rsPrebuilt s)
          (filter rsUpToDate rebuildStatuses)

  -- other modules have to be (may have to be) built
  let rebuildMap =
        M.fromList $
          (\s -> (rsModuleName s, (rsPrebuilt s, rsUpToDate s)))
            <$> filter (not . flip M.member allPrebuilt . rsModuleName) rebuildStatuses
  let toBeRebuilt = M.keys rebuildMap

  -- set of all dependencies of modules to be rebuilt
  let allBuildDeps = S.unions (S.fromList . moduleDeps <$> toBeRebuilt)
  let inBuildDeps = flip S.member allBuildDeps

  -- we only need prebuilts for deps of the modules to be build
  let toLoadPrebuilt = M.mapWithKey const $
        if preloadAll
          then allPrebuilt
          else M.filterWithKey (const . inBuildDeps) allPrebuilt

  -- we will need previously built results for modules to be build
  -- to skip rebuilding if deps have not changed
  let toLoadPrev =
        M.mapMaybeWithKey
          ( \mn (prev, upToDate) ->
              -- also exclude from loading changed modules that have no
              -- dependants (we actually need to check only against up to date
              -- dependants, but we check here against all)
              if isNothing prev || (not upToDate && (not . inBuildDeps) mn)
                then Nothing
                else Just (upToDate, mn)
          )
          rebuildMap

  (prebuiltLoad, prevLoad) <-
    A.concurrently
      (A.forConcurrently toLoadPrebuilt loadPrebuilt)
      (A.forConcurrently toLoadPrev (\(up, mn) -> fmap (up,) <$> loadPrebuilt mn ))

  let prebuilt = M.mapMaybe id prebuiltLoad
  let previous = M.mapMaybe id prevLoad

  -- evaluate $ trace ("toBeRebuilt" <> show toBeRebuilt) ()
  -- evaluate $ trace ("prebuilt:" <> show (M.keys prebuilt)) ()
  -- evaluate $ trace ("previous:" <> show (M.keys previous)) ()

  -- if for some reason (wrong version, files corruption) loading fails,
  -- those modules should be rebuilt too
  let failedLoads = M.keys $ M.filter isNothing prebuiltLoad
  buildJobs <- foldM makeBuildJob M.empty (toBeRebuilt <> failedLoads)

  env <- C.newMVar primEnv
  idx <- C.newMVar 1
  pure
    ( BuildPlan prebuilt previous buildJobs env idx
    , let
        update = flip $ \s ->
          M.alter (const (rsNewCacheInfo s)) (rsModuleName s)
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
            { rsModuleName = moduleName
            , rsRebuildNever = True
            , rsPrebuilt = timestamp
            , rsUpToDate = True
            , rsNewCacheInfo = Nothing
            })
        Left RebuildAlways -> do
          pure (RebuildStatus
            { rsModuleName = moduleName
            , rsRebuildNever = False
            , rsPrebuilt = Nothing
            , rsUpToDate = False
            , rsNewCacheInfo = Nothing
            })
        Right cacheInfo -> do
          cwd <- liftBase getCurrentDirectory
          (newCacheInfo, isUpToDate) <- checkChanged cacheDb moduleName cwd cacheInfo
          timestamp <- getOutputTimestamp moduleName
          pure (RebuildStatus
            { rsModuleName = moduleName
            , rsRebuildNever = False
            , rsPrebuilt = timestamp
            , rsUpToDate = isUpToDate
            , rsNewCacheInfo = Just newCacheInfo
            })

    moduleDeps = fromMaybe graphError . flip lookup graph
      where
        graphError = internalError "make: module not found in dependency graph."

    collectPrebuiltModules :: M.Map ModuleName UTCTime -> (ModuleName, Bool, UTCTime) -> M.Map ModuleName UTCTime
    collectPrebuiltModules prev (moduleName, rebuildNever, pb)
      | rebuildNever = M.insert moduleName pb prev
      | otherwise = do
          let deps = moduleDeps moduleName
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
