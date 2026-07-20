{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.Ide.Rebuild
  ( rebuildFileSync
  , rebuildFileAsync
  , rebuildManyAsync
  , rebuildOneAsync
  , rebuildAll
  --, rebuildOne
  , rebuildFile
  , dayZero
  ) where

import Protolude hiding (hash, moduleName, trace)

import Control.Concurrent.Async.Lifted qualified as A
import Data.List qualified as List
import Data.Map.Lazy qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Text qualified as Text
import Data.Text.Encoding as TSE
import Data.Time qualified as Time
import Language.PureScript qualified as P
import Language.PureScript.CST qualified as CST
import Language.PureScript.CoreFn qualified as CF
import Language.PureScript.Docs.Types qualified as Docs
import Language.PureScript.Glob (PSCGlobs (..), toInputGlobs)
import Language.PureScript.Make (MakeOptions (..), ffiCodegen')
import Language.PureScript.Make.ExternsDiff qualified as ED
import Language.PureScript.Make.Cache (CacheDb, CacheInfo (..), checkChanged, hash, normaliseForCache)
import Language.PureScript.Make.Monad (getCurrentTime, hashFile)
import "monad-logger" Control.Monad.Logger (LoggingT, MonadLogger, logDebug, logDebugN, logInfoN)

import Data.IORef.Lifted as Ref
import Language.PureScript.Ide.Error (IdeError (..))
import Language.PureScript.Ide.Logging (labelTimespec, logPerf, runLoggerWithFile)
import Language.PureScript.Ide.SourceFile (parseModulesFromFiles')
import Language.PureScript.Ide.State (cacheRebuild, getFileState, insertExterns, insertModule, populateVolatileState, updateCacheTimestamp)
import Language.PureScript.Ide.State qualified as S
import Language.PureScript.Ide.Types (Ide, IdeConfiguration (..), IdeEnvironment (..), IdeFileState (..), ModuleMap, Success (..))
import Language.PureScript.Ide.Util (ideReadFile)
import System.Directory (getCurrentDirectory)
import Language.PureScript.Externs (ExternsFile(efModuleName))



-- | Given a filepath performs the following steps:
--
-- * Reads and parses a PureScript module from the filepath.
--
-- * Builds a dependency graph for the parsed module from the already loaded
-- ExternsFiles.
--
-- * Attempts to find an FFI definition file for the module by looking
-- for a file with the same filepath except for a .js extension.
--
-- * Passes all the created artifacts to @rebuildModule@.
--
-- * If the rebuilding succeeds, returns a @RebuildSuccess@ with the generated
-- warnings, and if rebuilding fails, returns a @RebuildError@ with the
-- generated errors.
rebuildFile ::
  (Ide m, MonadLogger m, MonadError IdeError m) =>
  -- | The file to rebuild
  FilePath ->
  -- | The file to use as the location for parsing and errors
  Maybe FilePath ->
  -- | The targets to codegen
  Set P.CodegenTarget ->
  -- | A runner for the second build with open exports
  (ReaderT IdeEnvironment (LoggingT IO) () -> m ()) ->
  m Success
rebuildFile file actualFile codegenTargets runOpenBuild = do
  (fp, input) <-
    case List.stripPrefix "data:" file of
      Just source -> pure ("", Text.pack source)
      _ -> ideReadFile file
  let fp' = fromMaybe fp actualFile
  (pwarnings, m) <- case sequence $ CST.parseFromFile fp' input of
    Left parseError ->
      throwError $ RebuildError [(fp', input)] $ CST.toMultipleErrors fp' parseError
    Right m -> pure m

  let pwarnings' = CST.toMultipleWarnings fp pwarnings
  let moduleName = P.getModuleName m
  -- Externs files must be sorted ahead of time, so that they get applied
  -- in the right order (bottom up) to the 'Environment'.
  -- externs <- logPerf (labelTimespec "Sorting externs") (sortExterns m =<< getExternFiles)
  externs <-
    logPerf
      (labelTimespec "Sorting externs")
      (getFileState >>= sortExterns m . ((,) <$>  (map snd . fsExterns) <*> fsModules))
  outputDirectory <- confOutputPath <$> S.readIdeConfiguration

  -- For rebuilding, we want to 'RebuildAlways', but for inferring foreign
  -- modules using their file paths, we need to specify the path in the 'Map'.
  let filePathMap = M.singleton moduleName (Left P.RebuildAlways)
  let pureRebuild = fp == ""
  let modulePath = if pureRebuild then fp' else file
  foreigns <- P.inferForeignModules (M.singleton moduleName (Right modulePath))
  let makeEnv =
        P.buildMakeActions outputDirectory filePathMap foreigns False
          & (if pureRebuild then enableForeignCheck foreigns codegenTargets . shushCodegen else identity)
          & shushProgress
  -- Rebuild the single module using the cached externs
  (result, warnings) <- logPerf (labelTimespec $ "Rebuilding Module" <> " (pure: " <> show pureRebuild <> ")") $
    liftIO $ P.runMake (P.defaultOptions {P.optionsCodegenTargets = codegenTargets}) do
      newExterns <- P.rebuildModule makeEnv externs (pwarnings, m)
      unless pureRebuild $
        updateCacheDb codegenTargets outputDirectory file actualFile moduleName
      pure newExterns
  case result of
    Left errors ->
      throwError (RebuildError [(fp', input)] errors)
    Right newExterns -> do
      insertModule (fromMaybe file actualFile, m)
      insertExterns (dayZero, newExterns)
      void populateVolatileState
      _ <- updateCacheTimestamp
      runOpenBuild (rebuildModuleOpen makeEnv externs m)
      pure (RebuildSuccess (pwarnings' <> warnings))

-- | When adjusting the cache db file after a rebuild we always pick a
-- non-sensical timestamp ("1858-11-17T00:00:00Z"), and rely on the
-- content hash to tell whether the module needs rebuilding. This is
-- because IDE rebuilds may be triggered on temporary files to not
-- force editors to save the actual source file to get at diagnostics
dayZero :: Time.UTCTime
dayZero = Time.UTCTime (Time.ModifiedJulianDay 0) 0

updateCacheDb ::
  (MonadIO m) =>
  (MonadError P.MultipleErrors m) =>
  Set P.CodegenTarget ->
  -- | The output directory
  FilePath ->
  -- | The file to read the content hash from
  FilePath ->
  -- | The file name to update in the cache
  Maybe FilePath ->
  -- | The module name to update in the cache
  P.ModuleName ->
  m ()
updateCacheDb codegenTargets outputDirectory file actualFile moduleName = do
  cwd <- liftIO getCurrentDirectory
  contentHash <- P.hashFile file
  let moduleCacheInfo = (normaliseForCache cwd (fromMaybe file actualFile), (dayZero, contentHash))

  foreignCacheInfo <-
    if S.member P.JS codegenTargets
      then do
        foreigns' <- P.inferForeignModules (M.singleton moduleName (Right (fromMaybe file actualFile)))
        for (M.lookup moduleName foreigns') \foreignPath -> do
          foreignHash <- P.hashFile foreignPath
          pure (normaliseForCache cwd foreignPath, (dayZero, foreignHash))
      else pure Nothing

  let cacheInfo = M.fromList (moduleCacheInfo : maybeToList foreignCacheInfo)
  cacheDb <- P.readCacheDb' outputDirectory
  P.writeCacheDb' outputDirectory (M.insert moduleName (CacheInfo cacheInfo) cacheDb)

rebuildFileAsync ::
  forall m.
  (Ide m, MonadLogger m, MonadError IdeError m) =>
  FilePath ->
  Maybe FilePath ->
  Set P.CodegenTarget ->
  m Success
rebuildFileAsync fp fp' ts = rebuildFile fp fp' ts asyncRun
  where
    asyncRun = (ask >>=) . flip asyncRunWithEnv

rebuildFileSync ::
  forall m.
  (Ide m, MonadLogger m, MonadError IdeError m) =>
  FilePath ->
  Maybe FilePath ->
  Set P.CodegenTarget ->
  m Success
rebuildFileSync fp fp' ts = rebuildFile fp fp' ts syncRun
  where
    syncRun = (ask >>=) . flip syncRunWithEnv

rebuildManyAsync ::
  forall m.
  (Ide m, MonadLogger m, MonadError IdeError m) =>
  [(FilePath, Text)] ->
  Set P.CodegenTarget ->
  m Success
rebuildManyAsync files ts = rebuildMany files ts

rebuildOneAsync ::
  forall m.
  (Ide m, MonadLogger m, MonadError IdeError m) =>
  (FilePath, Text) ->
  Set P.CodegenTarget ->
  m Success
rebuildOneAsync (file, text) ts = rebuildOne file text ts asyncRun
  where
    asyncRun = (ask >>=) . flip asyncRunWithEnv

-- | Rebuilds a module but opens up its export list first and stores the result
-- inside the rebuild cache
rebuildModuleOpen ::
  (Ide m, MonadLogger m) =>
  P.MakeActions P.Make ->
  [P.ExternsFile] ->
  P.Module ->
  m ()
rebuildModuleOpen makeEnv externs m = void $ runExceptT do
  (openResult, _) <-
    liftIO $
      P.runMake P.defaultOptions $
        P.rebuildModule (shushProgress (shushCodegen makeEnv)) externs (mempty, openModuleExports m)
  case openResult of
    Left _ ->
      throwError (GeneralError "Failed when rebuilding with open exports")
    Right result -> do
      $(logDebug)
        ("Setting Rebuild cache: " <> P.runModuleName (P.efModuleName result))
      cacheRebuild result

-- | Shuts the compiler up about progress messages
shushProgress :: (Monad m) => P.MakeActions m -> P.MakeActions m
shushProgress ma =
  ma {P.progress = \_ -> pure ()}

-- | Stops any kind of codegen
shushCodegen :: (Monad m) => P.MakeActions m -> P.MakeActions m
shushCodegen ma =
  ma
    { P.codegen = \_ _ _ _ -> pure ()
    , P.ffiCodegen = \_ -> pure ()
    }

-- | Enables foreign module check without actual codegen.
enableForeignCheck ::
  M.Map P.ModuleName FilePath ->
  S.Set P.CodegenTarget ->
  P.MakeActions P.Make ->
  P.MakeActions P.Make
enableForeignCheck foreigns codegenTargets ma =
  ma
    { P.ffiCodegen = ffiCodegen' True foreigns codegenTargets Nothing
    }

getWithNotFound :: P.MultipleErrors -> [P.ModuleName]
getWithNotFound =
  mapMaybe P.errorModule .
    (filter (isModuleFoundFound . P.unwrapErrorMessage) . P.runMultipleErrors)
  where
  isModuleFoundFound (P.ModuleNotFound _) = True
  isModuleFoundFound _ = False

-- | Returns a topologically sorted list of dependent ExternsFiles for the given
-- module. Throws an error if there is a cyclic dependency within the
-- ExternsFiles
sortExterns ::
  (Ide m, MonadError IdeError m, MonadLogger m) =>
  P.Module ->
  -- to supply full module with paths ect.
  (ModuleMap P.ExternsFile, ModuleMap (P.Module, FilePath)) ->
  m [P.ExternsFile]
sortExterns m (ex, ms) = do
  sorted' <-
    runExceptT
      . P.sortModules P.Transitive P.moduleSignature
      . (:) m
      . map mkModule
      . M.elems
      . M.delete (P.getModuleName m)
      $ ex
  case sorted' of
    Left err -> do
      _ <- logInfoN $ "sortExterns error " <> show err
      -- Filter out modules with ModuleNotFound errors and try to sort again.
      let errorMs = M.fromList $ map (, ()) $ getWithNotFound err
      if M.null errorMs || M.member (P.getModuleName m) errorMs then
        throwError (RebuildError [] err)
      else do
        _ <- logInfoN $ "sortExterns removing errored modules: "  <> show (M.keys errorMs)
        sortExterns m (M.difference ex errorMs, ms)

    Right (sorted, graph) -> do
      let deps = fromJust (List.lookup (P.getModuleName m) graph)
      pure $ mapMaybe getExtern (deps `inOrderOf` map P.getModuleName sorted)
  where
    mkShallowModule P.ExternsFile {..} =
      P.Module (P.internalModuleSourceSpan "<rebuild>") [] efModuleName (map mkImport efImports) Nothing
    mkModule ef@P.ExternsFile {..} = maybe (mkShallowModule ef) fst (M.lookup efModuleName ms)
    mkImport (P.ExternsImport mn it iq) =
      P.ImportDeclaration (P.internalModuleSourceSpan "<rebuild>", []) mn it iq
    getExtern mn = M.lookup mn ex
    -- Sort a list so its elements appear in the same order as in another list.
    inOrderOf :: (Ord a) => [a] -> [a] -> [a]
    inOrderOf xs ys = let s = S.fromList xs in filter (`S.member` s) ys

-- | Removes a modules export list.
openModuleExports :: P.Module -> P.Module
openModuleExports (P.Module ss cs mn decls _) = P.Module ss cs mn decls Nothing

makePartial :: P.Module -> CST.PartialResult P.Module
makePartial m =
  CST.PartialResult m ([], Right m)

runWithEnv :: (MonadIO m) => (IO () -> IO a) -> IdeEnvironment -> ReaderT IdeEnvironment (LoggingT IO) () -> m ()
runWithEnv mode env action = do
  let ll = confLogLevel (ideConfiguration env)
  let logHandle = ideLogHandle env
  void (liftIO (mode (runLoggerWithFile ll logHandle (runReaderT action env))))

syncRunWithEnv :: (MonadIO m) => IdeEnvironment -> ReaderT IdeEnvironment (LoggingT IO) () -> m ()
syncRunWithEnv = runWithEnv identity

asyncRunWithEnv :: (MonadIO m) => IdeEnvironment -> ReaderT IdeEnvironment (LoggingT IO) () -> m ()
asyncRunWithEnv = runWithEnv async

rebuildCodegen ::
  IdeEnvironment ->
  Maybe (Ref.IORef [P.ModuleName]) ->
  CF.Module CF.Ann ->
  Docs.Module ->
  P.ExternsFile ->
  P.MultipleErrors ->
  P.SupplyT P.Make ()
rebuildCodegen env compiledRef ann docs externs warnings = do
    let mn = CF.moduleName ann
    --lift $ Ref.modifyIORef compiledRef ((:) mn)
    lift $ maybe (pure ()) (flip Ref.modifyIORef ((:) mn)) compiledRef
    lift $ syncRun do
      curTime <- getCurrentTime
      S.insertExterns (curTime, externs)
      S.insertWarnings mn warnings
      S.insertModuleUpdate mn (Right (curTime, ann, docs, externs, warnings))
  where
    syncRun = syncRunWithEnv env

rebuildProgress :: IdeEnvironment -> Ref.IORef [P.ModuleName] -> P.ProgressMessage -> P.Make ()
rebuildProgress env compiledRef pm = do
  case pm of
    P.CompilingModule mn _ _ ->
      liftIO $ Ref.modifyIORef compiledRef ((:) mn)
    P.SkippingModule mn _ ->
      -- Place skipped in to compiled to notify that we checked that is .
      liftIO $ Ref.modifyIORef compiledRef ((:) mn)
    _ ->
      pure ()
  syncRun $ logDebugN (P.renderProgressVerboseMessage "" pm)
  where
    syncRun = syncRunWithEnv env

rebuildOne ::
  (Ide m, MonadLogger m, MonadError IdeError m) =>
  FilePath ->
  Text ->
  Set P.CodegenTarget ->
  (ReaderT IdeEnvironment (LoggingT IO) () -> m ()) ->
  m Success
rebuildOne fp input codegenTargets runAfter = do

  (pwarnings, m) <- case sequence $ CST.parseFromFile fp input of
    Left parseError ->
      throwError $ Rebuild2Error [] (mempty, CST.toMultipleErrors fp parseError)
    Right m -> pure m

  let pwarnings' = CST.toMultipleWarnings fp pwarnings
  let moduleName = P.getModuleName m
  -- Externs files must be sorted ahead of time, so that they get applied
  -- in the right order (bottom up) to the 'Environment'.
  -- externs <- logPerf (labelTimespec "Sorting externs") (sortExterns m =<< getExternFiles)
  let toRebuild2Error = \case
        RebuildError _ err -> throwError $ Rebuild2Error [fp] (mempty, err)
        err -> throwError err
  externsEnv <- flip catchError toRebuild2Error $ do
        logPerf
          (labelTimespec "Sorting externs")
          (getFileState >>= sortExterns m . ((,) <$> (map snd . fsExterns) <*> fsModules))

  outputDirectory <- confOutputPath <$> S.readIdeConfiguration
  -- For rebuilding, we want to 'RebuildAlways', but for inferring foreign
  -- modules using their file paths, we need to specify the path in the 'Map'.
  let filePathMap = M.singleton moduleName (Left P.RebuildAlways)

  curExterns <- M.lookup moduleName . fsExterns <$> getFileState

  let modulePath = fp
  foreigns <- P.inferForeignModules (M.singleton moduleName (Right modulePath))
  let usePrefix = False

  env <- ask
  let syncRun = syncRunWithEnv env

  let makeEnv =
        (P.buildMakeActions outputDirectory filePathMap foreigns usePrefix)
          { P.codegen = \ann docs externs warnings -> do
            let mn = CF.moduleName ann

            lift $ syncRun do
              curTime <- getCurrentTime
              --insertExterns (curTime, externs)

              logDebugN $ "RebuildOne externs diff" <> show (ED.diffExterns [] externs . snd <$> curExterns )

              -- If no externs diff there is no need to rebuild deps. And to
              -- avoid recompilation of the module in rebuildMany we treat these
              -- new results as actual. We update cache-db and externs with the
              -- old timestamp (not to be later dependency) as if this module
              -- was built with those results then.
              case curExterns  of
                Just (extTs, curExt) | ED.isEmpty (ED.diffExterns [] externs curExt) -> do

                  cacheDb <- fsCacheDb <$> getFileState
                  cwd <- liftIO getCurrentDirectory

                  let
                    contentHash = hash (TSE.encodeUtf8 input)
                    cacheFp = normaliseForCache cwd fp

                  let newCacheDb = M.alter
                        \case
                          Just (CacheInfo info) ->
                            -- Just update fp if present.
                            Just $ CacheInfo $ M.update (Just . const (curTime, contentHash)) cacheFp info
                          Nothing ->
                            Just (CacheInfo $ M.singleton cacheFp (curTime, contentHash) )
                        moduleName  cacheDb
                  S.insertCacheDb newCacheDb
                  S.insertModuleUpdate mn (Right (extTs, ann, docs, externs, warnings))

                  -- We insert new externs because as diff is empty they do not
                  -- require rebuild of dependencies.
                  insertExterns (extTs, externs)
                  S.insertWarnings mn warnings

                Just _ ->
                    pure ()

                Nothing -> do
                  -- If no externs lets add them, it is needed when make can not
                  -- be performed (i.e. no needed modules)
                  insertExterns (curTime, externs)
                  S.insertWarnings mn warnings
                  pure ()
          }
          -- & shushCodegen
          & enableForeignCheck foreigns (S.singleton P.JS)

  -- Rebuild the single module using the cached externs
  (result, warnings) <- -- logPerf (labelTimespec $ "RebuildOne") $
    liftIO $ P.runMake (P.defaultOptions {P.optionsCodegenTargets = codegenTargets}) do
      P.rebuildModule makeEnv externsEnv (pwarnings, m)

  S.insertModule (fp, m)

  case result of
    Left errors -> do
      cacheDb <- fsCacheDb <$> getFileState
      S.insertCacheDb (M.delete moduleName cacheDb)
      throwError (Rebuild2Error [fp] (pwarnings' <> warnings, errors))
    Right _ -> do
      void populateVolatileState
      -- insertModule (fp, m)
      -- insertExterns newExterns
      --void populateVolatileState
      --_ <- updateCacheTimestamp
      runAfter do
        rebuildModuleOpen makeEnv externsEnv m
        S.cleanUpModule (fp, m)

      pure (Rebuild2Result [fp] (pwarnings' <> warnings, mempty))

rebuildMany ::
  (Ide m, MonadLogger m, MonadError IdeError m) =>
  -- | The files to rebuild
  [(FilePath, Text)] ->
  -- | The targets to codegen
  Set P.CodegenTarget ->
  m Success
rebuildMany files targets = do
  parsedMap <-
    M.fromList <$> for
      files
      \(fp, input) -> do
        (warnings, m) <- case sequence $ CST.parseFromFile fp input of
          Left parseError ->
            throwError $ Rebuild2Error [] (mempty, CST.toMultipleErrors fp parseError)
          Right m -> pure m
        pure (P.getModuleName m, (warnings, m, fp, input))


  let foreignFpMap = parsedMap <&> (\(_, _, fp, _) -> Right fp)
  let inputModulesMap = parsedMap <&> (\(_, m, fp, _) -> (fp, m))
  let inputTextMap = parsedMap <&> (\(_, _, fp, input) -> (fp, input))

  traverse_ insertModule inputModulesMap

  (modules, allExterns, cacheDb, updates) <-
    getFileState <&> \s ->
      (fsModules s, fsExterns s, fsCacheDb s, fsUpdates s)

  -- let allModulesMap = M.union inputModulesMap modules
  let partials = makePartial . fst <$> M.elems modules
  let allFilePathMap = snd <$> modules

  --print foreignFpMap
  -- Foreigns only for input modules.
  foreigns <- P.inferForeignModules foreignFpMap

  --print $ foreigns

  env <- ask
  let syncRun = syncRunWithEnv env

  compiledRef <- liftIO $ Ref.newIORef []

  timestamp <- getCurrentTime
  let makeEnv =
        P.MakeActions
          { P.getInputTimestampsAndHashes = \mn -> do
              pure
                case M.lookup mn inputTextMap of
                  Just (fp, input) ->
                    Right $
                      M.singleton fp (timestamp, pure (hash (TSE.encodeUtf8 input)))
                        & case M.lookup mn foreigns of
                          Just ffp -> M.insert ffp (timestamp, hashFile ffp)
                          _ -> identity
                  --_ -> case M.lookup mn cacheDb <|> M.lookup mn cacheDbRemoved of
                  _ -> case M.lookup mn cacheDb of
                    Just (CacheInfo {..}) -> Right $ map pure <$> unCacheInfo
                    Nothing -> Left P.RebuildAlways
          , P.getOutputTimestamp = \mn -> pure do
              case M.lookup mn updates of
                Just (Left time) -> Just time
                Just (Right (time, _, _, _, _)) -> Just time
                Nothing -> fst <$> M.lookup mn allExterns
          , -- Externs fileName not relevant for now
            P.readExterns = pure . ("",) . map snd . flip M.lookup allExterns
          --, P.readWarnings = \(mn, _) -> pure ("", M.lookup mn loadedWarnings)
          , P.readWarnings = \(mn, _) -> pure ("", Nothing)
          , P.readCacheDb = pure cacheDb
          , P.updateOutputTimestamp = \mn _ -> do
              syncRun do
                curTime <- getCurrentTime
                S.insertModuleUpdate mn (Left curTime)
                S.updateExternsTimestamp mn curTime
              pure True
          , P.codegen = rebuildCodegen env Nothing --(Just compiledRef)
          , P.ffiCodegen = \_ -> pure ()
          , P.writeCacheDb = \newCacheDb -> do
              syncRun do
                S.insertCacheDb newCacheDb
                --S.insertCacheDbRemoved (M.union (M.difference cacheDb newCacheDb) cacheDbRemoved)
          , P.writePackageJson = pure ()
          , P.outputPrimDocs = pure ()
          , P.progress = rebuildProgress env compiledRef
          } & enableForeignCheck foreigns targets
  -- Actual seems we can use just "make_"? (though it assumes gathering warnings)
  -- Collect: False we don't need as we gather it in the codegen handler.
  let makeOptions = MakeOptions {moCollectAllExterns = False}

  (makeResult, warnings) <- logPerf (labelTimespec "RebuildMany make") $
    liftIO $ runMakeWithNotFound P.defaultOptions
      (P.make' makeOptions makeEnv) partials

  unless (null targets) do
    writeUpdates targets

  compiledModules <- liftIO $ Ref.readIORef compiledRef
  let getFilePath = flip M.lookup allFilePathMap
  let compiled = mapMaybe getFilePath compiledModules

  -- For now, before make if modified to return only warnings for compiled,
  -- take warnings from fsUpdates, not from make results. But only in case of for success
  -- newUpdates <- fsUpdates <$> getFileState
  -- let warnings' = foldMap
  --         ( \mn -> maybe mempty (\case
  --               Right (_, _, _, _, w) -> w
  --               Left _ -> mempty) (M.lookup mn newUpdates)
  --         )
  --         compiledModules

  -- As we place new warnings in fsWarnings we may get it from there.
  newWarnings <- fsWarnings <$> getFileState
  let warnings' = foldMap (fromMaybe mempty . flip M.lookup newWarnings) compiledModules

  -- We should do it in any case because there could be successful compilations
  -- when error.
  void populateVolatileState

  case makeResult of
    Left errors ->
      -- throwError (RebuildError [(fp, input)] errors)
      pure (Rebuild2Result compiled (warnings, errors))
    Right _ -> do
      --void populateVolatileState

      -- Rebuild the first for now?
      -- case M.toList inputModulesMap of
      --   ((_, (_, m)) : _) ->
      --     runOpenBuild (rebuildModuleOpen makeEnv newExterns m)
      --   _ ->
      --     pure ()
      -- pure (Rebuild2Result (CST.toMultipleWarnings fp pwarnings <> warnings))
      pure (Rebuild2Result compiled (warnings', mempty))

-- | Finds all matches for the globs specified at the commandline
findAllSourceFiles :: (Ide m) => m [FilePath]
findAllSourceFiles = do
  IdeConfiguration {..} <- S.readIdeConfiguration
  liftIO $
    toInputGlobs $
      PSCGlobs
        { pscInputGlobs = confGlobs
        , pscInputGlobsFromFile = confGlobsFromFile
        , pscExcludeGlobs = confGlobsExclude
        , pscWarnFileTypeNotFound = const $ pure ()
        }

-- TODO: Add this to rebuildMany too.
runMakeWithNotFound
  :: P.Options
  -> ([CST.PartialResult P.Module] -> P.Make [P.ExternsFile])
  -> [CST.PartialResult P.Module]
  -> IO (Either P.MultipleErrors [P.ExternsFile], P.MultipleErrors)
runMakeWithNotFound options make = go []
  where
    ret errs (res, warnings) = pure
      case errs of
        -- Return ModuleNotFound errors only for first time Make run. Because we
        -- don't want to to propagate module absence though the codebase. Though
        -- maybe this is a subject for consideration. Though compilation was
        -- successful we still return the ModuleNotFound error, we may do this
        -- as we do not collect compiled externs from Make result.
        (err : _) ->
          (either (Left . (<>) err) (const $ Left err) res, warnings)
        _ ->
          (res, warnings)

    go notFoundErrs modules = do
      (res, warnings) <- P.runMake options $ make modules
      case res of
        -- If this returns ModuleNotFound error this means actual compilation
        -- was not run, we wil remove all the failed modules and try again.
        Left err | ms@(_ : _) <- getWithNotFound err-> do
          -- Filter out modules with ModuleNotFound errors and run again.
          --print (show (map (P.getModuleName . CST.resPartial) modules) :: Text)
          go (notFoundErrs <> [err])
            (filter (not . flip elem ms . P.getModuleName . CST.resPartial) modules)
        _ ->
          ret notFoundErrs (res, warnings)

-- | Rebuild
-- - Reads all modules sources
-- - Parses all sources
--
rebuildAll ::
  (Ide m, MonadLogger m, MonadError IdeError m) =>
  Set P.CodegenTarget ->
  m Success
rebuildAll codegenTargets = do
  -- Gets relative paths.
  sourceFiles <- logPerf (labelTimespec "findAllSourceFiles") findAllSourceFiles

  -- Load modules and parse all.
  (partialParseErrors, allPartialWithText) <- logPerf (labelTimespec "parseModulesFromFiles")  $
    partitionEithers <$> parseModulesFromFiles' sourceFiles

  let partialModules = (\(f, t, m) -> (f, m)) <$> allPartialWithText

  -- Parse full modules.
  let (fullParseErrors, modules) =
        partitionEithers $
          (\(fp, r) -> either (Left . CST.toMultipleErrors fp) (Right . (fp,)) r)
            <$> (map (snd . CST.resFull) <$> partialModules)

  -- Insert fully parsed modules into ide state.
  logPerf (labelTimespec "insertModules")  do
      --traverse_ insertModule modules
      S.insertModules modules


  let allFilePathMap =
        M.fromList $
          map (\(fp, pm) -> (P.getModuleName $ CST.resPartial pm, fp)) partialModules

  let filePathMap = Right <$> allFilePathMap

  -- Find foreigns for all modules.
  foreigns <- P.inferForeignModules filePathMap
  outputDirectory <- S.getOutputDirectory

  let options = P.defaultOptions
  let usePrefix = False
  let defaultActions = P.buildMakeActions outputDirectory filePathMap foreigns usePrefix

  let allModules = M.keysSet filePathMap

  modulesToLoad <- S.difference allModules . S.fromList <$> S.getLoadedModuleNames

  -- Load all timestamps and externs from disk for all modules (that are not loaded in state).
  loadedExterns <- map (fromMaybe [] . hush . fst) $ liftIO $ P.runMake options $
    catMaybes
      <$> flip A.mapConcurrently (S.toList modulesToLoad)
        \mn ->
          P.getOutputTimestamp defaultActions mn >>=
            maybe (pure Nothing) (\ts -> map (ts,) . snd <$> P.readExterns defaultActions mn)


  A.mapConcurrently_ insertExterns loadedExterns

  (_, allExterns, curCacheDb, updates) <-
    getFileState <&> \s ->
      (fsModules s, fsExterns s, fsCacheDb s, fsUpdates s)

  cacheDbLoaded <- fromMaybe M.empty . hush . fst <$>
    liftIO (P.runMake options $ P.readCacheDb defaultActions)
  let cacheDb = M.union curCacheDb  cacheDbLoaded
  -- Insert cache-db in case we fail to do this in while running make.
  S.insertCacheDb cacheDb

  env <- ask
  let syncRun = syncRunWithEnv env

  compiledRef <- liftIO $ Ref.newIORef []

  let makeActions =
        P.MakeActions
          { -- TODO: Use hashes from loaded texts (foreigns wont' be needed in actions)
            -- For this we would need real input timestamps also.
            P.getInputTimestampsAndHashes = P.getInputTimestampsAndHashes defaultActions
          --, P.getOutputTimestamp = P.getOutputTimestamp defaultActions
          --, P.getOutputTimestamp = pure . map fst . flip M.lookup allExterns
          , P.getOutputTimestamp = \mn -> pure
              case M.lookup mn updates of
                Just (Left time) -> Just time
                Just (Right (time, _, _, _, _)) -> Just time
                Nothing -> fst <$> M.lookup mn allExterns
          --, P.readExterns = P.readExterns defaultActions
          , P.readExterns = pure . ("",) . map snd . flip M.lookup allExterns
          --, P.readWarnings = P.readWarnings defaultActions
          , P.readWarnings = \_ -> pure ("", Nothing)
          , P.readCacheDb = pure cacheDb
          , P.updateOutputTimestamp = \mn _ -> do
              syncRun do
                curTime <- getCurrentTime
                S.insertModuleUpdate mn (Left curTime)
                S.updateExternsTimestamp mn curTime
              pure True
          , P.codegen = rebuildCodegen env Nothing --(Just compiledRef)
          , P.ffiCodegen = \_ -> pure ()
          , P.writeCacheDb = \newCacheDb -> do
              syncRun do
                S.insertCacheDb newCacheDb
          , -- S.insertCacheDbRemoved (M.union (M.difference cacheDb newCacheDb) cacheDbRemoved)
            P.writePackageJson = pure ()
          , P.outputPrimDocs = pure ()
          , P.progress = rebuildProgress env compiledRef
          } & enableForeignCheck foreigns codegenTargets

  -- Collect: False we don't need as we gather it in the codegen handler.
  let makeOptions = MakeOptions {moCollectAllExterns = False}
  (makeResult, warnings) <- logPerf (labelTimespec "rebuildAll: make")
    -- $ liftIO $ P.runMake options $ do
      --P.make' makeOptions makeActions (map snd partialModules)
      $ liftIO $ runMakeWithNotFound options
        (P.make' makeOptions makeActions) (map snd partialModules)

  unless (null codegenTargets) do
    writeUpdates codegenTargets

  let parseErr = fold fullParseErrors <> foldMap snd partialParseErrors

  let errorModules = getErrorModules makeResult

  compiledModules <- S.fromList <$> liftIO (Ref.readIORef compiledRef)
  --let compiledModulesNoErrors = S.difference compiledModules errorModules
  let getFilePath = flip M.lookup allFilePathMap

  logDebugN $ "Compiled " <> show (length compiledModules) <> " modules with make."


  -- Load warnings only non-error modules
  let lackingWarnModules = M.toList $ M.withoutKeys allFilePathMap compiledModules
  lackingWarningsList <- map (fromMaybe [] . hush . fst) $ liftIO $ P.runMake options $
    catMaybes <$>
       flip A.mapConcurrently lackingWarnModules
        \(mn, fp) -> map (mn,) . snd <$>
          P.readWarnings defaultActions (mn, fp)

  S.insertWarnings' lackingWarningsList
  let lackingWarnings = foldMap snd lackingWarningsList

  -- Load externs for failed modules too.
  let loadedModules = S.fromList $ efModuleName . snd <$> loadedExterns
  --let compiled = mapMaybe getFilePath $ S.toList allModules
  let compiled = mapMaybe getFilePath $ S.toList $ S.unions [loadedModules, compiledModules, errorModules]

  logPerf (labelTimespec "rebuildAll populateVolatileState") $
    void populateVolatileState

  case makeResult of
    Right _ -> do
      -- We don't put compiled externs here because we did this while codegen
      -- (though we may consider changing it).
      pure $ Rebuild2Result compiled (warnings <> lackingWarnings, parseErr)
    Left err -> do
      pure $ Rebuild2Result compiled (warnings <> lackingWarnings, parseErr <> err)
  where
    getErrorModules = \case
      Right _ -> S.empty
      Left err -> S.fromList $ mapMaybe P.errorModule (P.runMultipleErrors err)

getFilePathFromCacheDb :: Bool -> CacheDb -> P.ModuleName -> Maybe FilePath
getFilePathFromCacheDb isForeign cacheDb mn =
  M.lookup mn cacheDb
    >>= \CacheInfo {..} ->
      case M.toList $ M.filterWithKey (\fp _ -> isPath fp) unCacheInfo of
        ((fp, _) : _) -> Just fp
        [] -> Nothing
  where
    isPath = (if isForeign then not else identity) . isSuffixOf ".purs"

writeUpdates :: Set P.CodegenTarget -> (Ide m, MonadLogger m) => m ()
writeUpdates targets = do
  updatesMap <- S.getUpdates
  cacheDb <- S.getCacheDb

  outputDir <- confOutputPath <$> S.readIdeConfiguration

  let foreigns = M.mapMaybeWithKey (\mn _ -> getFilePathFromCacheDb True cacheDb mn) updatesMap

  let usePrefix = False
  let atomicMode = False
  let actions = P.buildMakeActions outputDir mempty foreigns usePrefix
  -- Use writeMode for FFI codegen to avoid move file permission errors.
  let writeFfiCodegen =
        ffiCodegen' atomicMode foreigns targets (Just $ P.makeOutputFilename outputDir)

  env <- ask
  let syncRun = syncRunWithEnv env

  if null updatesMap
    then logInfoN "No modules update in the output directory."
    else do
      let options = P.defaultOptions {P.optionsNoComments = True, P.optionsCodegenTargets = targets}
      -- curTime <- getCurrentTime
      -- We should handle errors might be thrown while codegen.
      (result, _) <- liftIO $ P.runMake options $ do
        void $ A.mapConcurrently
          ( \(mn, update) -> do
              timestamp <- case update of
                Right (timestamp, ann, docs, externs, warnings) -> do
                  syncRun $ logDebugN $ "Writing codegen update for " <> show mn
                  P.evalSupplyT 0 $ P.codegen actions ann docs externs warnings
                  writeFfiCodegen ann
                  pure timestamp
                Left timestamp -> do
                  pure timestamp

              -- We update all outputs of dumped modules (and their dependencies) to
              -- the same timestamp as we don't care about the actual dependency order
              -- and to avoid the problem of later dependencies.
              -- P.updateOutputTimestamp actions mn (Just curTime)
              void $ P.updateOutputTimestamp actions mn (Just timestamp)

          )
          (M.toList updatesMap)

        P.writeCacheDb actions cacheDb
        P.writePackageJson actions
        P.outputPrimDocs actions

      case result of
        Left err ->
          logInfoN ("Errors while writing updates to disk: " <> show err)
        Right () -> do
          _ <- updateCacheTimestamp

          S.cleanUpdates

          let allUpdates = M.size updatesMap
          let tsUpdates = M.size (M.filter isLeft updatesMap)
          let cgUpdates = allUpdates - tsUpdates

          logInfoN ("Updated " <> show allUpdates
            <> " (codegen/timestamp: " <> show cgUpdates <> "/"  <> show tsUpdates <> ")"
            <> " modules in the output directory.")