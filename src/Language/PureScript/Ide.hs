-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide
-- Description : Interface for the psc-ide-server
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Interface for the psc-ide-server
-----------------------------------------------------------------------------

{-# LANGUAGE PackageImports #-}

module Language.PureScript.Ide
       ( handleCommand
       ) where

import Protolude hiding (moduleName, trace)

import "monad-logger" Control.Monad.Logger (MonadLogger, logInfoN, logWarnN)
import Data.Map qualified as M
import Data.Text qualified as T
import Control.Concurrent.Async qualified as A
import Language.PureScript qualified as P
import Language.PureScript.Glob (toInputGlobs, PSCGlobs(..))
import Language.PureScript.CST qualified as CST
import Language.PureScript.Ide.CaseSplit qualified as CS
import Language.PureScript.Ide.Command (Command(..), ImportCommand(..), ListType(..))
import Language.PureScript.Ide.Completion (CompletionOptions, completionFromMatch, getCompletions, getExactCompletions, simpleExport)
import Language.PureScript.Ide.Error (IdeError(..))
import Language.PureScript.Ide.Externs (readExternFile)
import Language.PureScript.Ide.Filter (Filter)
import Language.PureScript.Ide.Imports (parseImportsFromFile)
import Language.PureScript.Ide.Imports.Actions (addImplicitImport, addImportForIdentifier, addQualifiedImport, answerRequest)
import Language.PureScript.Ide.Matcher (Matcher)
import Language.PureScript.Ide.Prim (idePrimDeclarations)
import Language.PureScript.Ide.Rebuild (rebuildFileAsync, rebuildFileSync)
import Language.PureScript.Ide.SourceFile (parseModulesFromFiles, parseModulesFromFiles')
import Language.PureScript.Ide.State (getAllModules, getLoadedModulenames, insertExterns, insertModule, populateVolatileState, populateVolatileStateSync, resetIdeState, updateCacheTimestamp)
import Language.PureScript.Ide.Types (Annotation(..), Ide, IdeConfiguration(..), IdeDeclarationAnn(..), IdeEnvironment(..), Success(..))
import Language.PureScript.Ide.Util (discardAnn, identifierFromIdeDeclaration, namespaceForDeclaration, withEmptyAnn)
import Language.PureScript.Ide.Usage (findUsages)
import System.Directory (getCurrentDirectory, getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), normalise)
import System.FilePath.Glob (glob)
import Language.PureScript (getModuleName)
import Language.PureScript.Make (inferForeignModules, buildMakeActions, MakeOptions (..))
import Data.List (intersect)

import Debug.Trace (trace)
-- | Accepts a Command and runs it against psc-ide's State. This is the main
-- entry point for the server.
handleCommand
  :: (Ide m, MonadLogger m, MonadError IdeError m)
  => Command
  -> m Success
handleCommand c = case c of
  Load [] ->
    -- Clearing the State before populating it to avoid a space leak
    --resetIdeState *> findAvailableExterns >>= loadModulesAsync
    resetIdeState *> loadModulesAsync []
  Load modules ->
    loadModulesAsync modules
  LoadSync [] ->
    findAvailableExterns >>= loadModulesSync
  LoadSync modules ->
    loadModulesSync modules
  Type search filters currentModule ->
    findType search filters currentModule
  Complete filters matcher currentModule complOptions ->
    findCompletions filters matcher currentModule complOptions
  List LoadedModules -> do
    logWarnN
      "Listing the loaded modules command is DEPRECATED, use the completion command and filter it to modules instead"
    printModules
  List AvailableModules ->
    listAvailableModules
  List (Imports fp) ->
    ImportList <$> parseImportsFromFile fp
  CaseSplit l b e wca t ->
    caseSplit l b e wca t
  AddClause l wca ->
    MultilineTextResult <$> CS.addClause l wca
  FindUsages moduleName ident namespace -> do
    M.lookup moduleName <$> getAllModules Nothing >>= \case
      Nothing -> throwError (GeneralError "Module not found")
      Just decls -> do
        case find (\d -> namespaceForDeclaration (discardAnn d) == namespace
                    && identifierFromIdeDeclaration (discardAnn d) == ident) decls of
          Nothing -> throwError (GeneralError "Declaration not found")
          Just declaration -> do
            let sourceModule = fromMaybe moduleName (declaration & _idaAnnotation & _annExportedFrom)
            UsagesResult . foldMap toList <$> findUsages (discardAnn declaration) sourceModule
  Import fp outfp _ (AddImplicitImport mn) -> do
    rs <- addImplicitImport fp mn
    answerRequest outfp rs
  Import fp outfp _ (AddQualifiedImport mn qual) -> do
    rs <- addQualifiedImport fp mn qual
    answerRequest outfp rs
  Import fp outfp filters (AddImportForIdentifier ident qual) -> do
    rs <- addImportForIdentifier fp ident qual filters
    case rs of
      Right rs' -> answerRequest outfp rs'
      Left question ->
        pure (CompletionResult (map (completionFromMatch . simpleExport . map withEmptyAnn) question))
  Rebuild file actualFile targets ->
    rebuildFileAsync file actualFile targets
  -- Just fake
  Rebuild2 file actualFile targets ->
    rebuildFileAsync file (Just $ T.unpack actualFile) targets
  RebuildSync file actualFile targets ->
    rebuildFileSync file actualFile targets
  Cwd ->
    TextResult . T.pack <$> liftIO getCurrentDirectory
  Reset ->
    resetIdeState $> TextResult "State has been reset."
  Quit ->
    liftIO exitSuccess

findCompletions
  :: Ide m
  => [Filter]
  -> Matcher IdeDeclarationAnn
  -> Maybe P.ModuleName
  -> CompletionOptions
  -> m Success
findCompletions filters matcher currentModule complOptions = do
  modules <- getAllModules currentModule
  let insertPrim = M.union idePrimDeclarations
  pure (CompletionResult (getCompletions filters matcher complOptions (insertPrim modules)))

findType
  :: Ide m
  => Text
  -> [Filter]
  -> Maybe P.ModuleName
  -> m Success
findType search filters currentModule = do
  modules <- getAllModules currentModule
  let insertPrim = M.union idePrimDeclarations
  pure (CompletionResult (getExactCompletions search filters (insertPrim modules)))

printModules :: Ide m => m Success
printModules = ModuleList . map P.runModuleName <$> getLoadedModulenames

outputDirectory :: Ide m => m FilePath
outputDirectory = do
  outputPath <- confOutputPath . ideConfiguration <$> ask
  cwd <- liftIO getCurrentDirectory
  pure (cwd </> outputPath)

listAvailableModules :: Ide m => m Success
listAvailableModules = do
  oDir <- outputDirectory
  liftIO $ do
    contents <- getDirectoryContents oDir
    let cleaned = filter (`notElem` [".", ".."]) contents
    return (ModuleList (map toS cleaned))

caseSplit :: (Ide m, MonadError IdeError m) =>
  Text -> Int -> Int -> CS.WildcardAnnotations -> Text -> m Success
caseSplit l b e csa t = do
  patterns <- CS.makePattern l b e csa <$> CS.caseSplit t
  pure (MultilineTextResult patterns)

-- | Finds all the externs inside the output folder and returns the
-- corresponding module names
findAvailableExterns :: (Ide m, MonadError IdeError m) => m [P.ModuleName]
findAvailableExterns = do
  oDir <- outputDirectory
  unlessM (liftIO (doesDirectoryExist oDir))
    (throwError (GeneralError $ "Couldn't locate your output directory at: " <> T.pack (normalise oDir)))
  liftIO $ do
    directories <- getDirectoryContents oDir
    moduleNames <- filterM (containsExterns oDir) directories
    pure (P.moduleNameFromString . toS <$> moduleNames)
  where
    -- Takes the output directory and a filepath like "Data.Array" and
    -- looks up, whether that folder contains an externs file
    containsExterns :: FilePath -> FilePath -> IO Bool
    containsExterns oDir d
      | d `elem` [".", ".."] = pure False
      | otherwise = do
          let file = oDir </> d </> P.externsFileName
          doesFileExist file

-- | Finds all matches for the globs specified at the commandline
findAllSourceFiles :: Ide m => m [FilePath]
findAllSourceFiles = do
  IdeConfiguration{..} <- ideConfiguration <$> ask
  liftIO $ toInputGlobs $ PSCGlobs
    { pscInputGlobs = confGlobs
    , pscInputGlobsFromFile = confGlobsFromFile
    , pscExcludeGlobs = confGlobsExclude
    , pscWarnFileTypeNotFound = const $ pure ()
    }


-- | Looks up the ExternsFiles for the given Modulenames and loads them into the
-- server state. Then proceeds to parse all the specified sourcefiles and
-- inserts their ASTs into the state. Finally kicks off an async worker, which
-- populates the VolatileState.
loadModulesAsync
  :: (Ide m, MonadError IdeError m, MonadLogger m)
  => [P.ModuleName]
  -> m Success
loadModulesAsync moduleNames = do
  tr <- loadModules moduleNames
  _ <- populateVolatileState
  pure tr

loadModulesSync
  :: (Ide m, MonadError IdeError m, MonadLogger m)
  => [P.ModuleName]
  -> m Success
loadModulesSync moduleNames = do
  tr <- loadModules moduleNames
  populateVolatileStateSync
  pure tr

loadModules_
  :: (Ide m, MonadError IdeError m, MonadLogger m)
  => [P.ModuleName]
  -> m Success
loadModules_ moduleNames = do
  -- We resolve all the modulenames to externs files and load these into memory.
  oDir <- outputDirectory
  let efPaths =
        map (\mn -> oDir </> toS (P.runModuleName mn) </> P.externsFileName) moduleNames
  efiles <- traverse readExternFile efPaths
  traverse_ insertExterns efiles

  -- We parse all source files, log eventual parse failures and insert the
  -- successful parses into the state.
  (failures, allModules) <-
    partitionEithers <$> (parseModulesFromFiles =<< findAllSourceFiles)
  unless (null failures) $
    logWarnN ("Failed to parse: " <> show failures)
  traverse_ insertModule allModules

  pure (TextResult ("Loaded " <> show (length efiles) <> " modules and "
                    <> show (length allModules) <> " source files."))

loadModules
  :: (Ide m, MonadError IdeError m, MonadLogger m)
  => [P.ModuleName]
  -> m Success
loadModules moduleNames_ = do
  -- We parse all source files, log eventual parse failures and insert the
  -- successful parses into the state.

  -- (failures_, allModules_) <-
  --   partitionEithers <$> (parseModulesFromFiles =<< findAllSourceFiles)

  -- Get partial modules.
  (failures, allPartial) <-
    partitionEithers <$> (parseModulesFromFiles' =<< findAllSourceFiles)

  -- Parse full modules.
  let (failures2, allModules) =
        partitionEithers $
          (\(fp, r) -> either (const (Left fp)) (Right . (fp,)) r)
            <$> (map (snd . CST.resFull) <$> allPartial)

  unless (null failures) $
    logWarnN ("Failed to parse: " <> show (failures <> failures2))
  traverse_ insertModule allModules

  let filePathMap = M.fromList $ map (\(fp, pm) -> (P.getModuleName $ CST.resPartial pm, Right fp)) allPartial
  foreigns <- inferForeignModules filePathMap

  outDir <- outputDirectory

  let usePrefix = False
  let makeActions = (buildMakeActions outDir filePathMap foreigns usePrefix)
        { P.progress = \_ -> pure () }

  -- let makeEnv = P.buildMakeActions outputDirectory filePathMap foreigns False
  --       & (if pureRebuild then enableForeignCheck foreigns codegenTargets . shushCodegen else identity)
  --       & shushProgress

  let options = P.defaultOptions { P.optionsNoComments = True}
  logInfoN ("Running make for: " <> show (length (map snd allPartial)))
  -- Run make without throwing.
  (result, warnings)  <- liftIO $ P.runMake options $ do
        P.make' (MakeOptions True False) makeActions (map snd allPartial)
  _ <- updateCacheTimestamp
  -- let efPaths =
  --       map (\mn -> outDir </> toS (P.runModuleName mn) </> P.externsFileName) modulesToLoad
  --externFiles_ <- A.mapConcurrently (\p -> runExceptT $ readExternFile p) efPaths
  -- externFiles <- liftIO $ catMaybes <$>
  --   A.mapConcurrently (map rightToMaybe . runExceptT . readExternFile) efPaths
  --externFiles <- traverse readExternFile efPaths
  let allModuleNames = map (P.getModuleName . CST.resPartial . snd) allPartial
  case result of
    Right externFiles -> do
        traverse_ insertExterns externFiles
        logInfoN ("Got externs from make: " <>  show (length externFiles) )

        -- Not optimal, of course.

        let lacking = filter (not . flip elem (P.efModuleName <$> externFiles)) allModuleNames


        pure (TextResult ("Loaded (with make) " <> show (length externFiles) <> " modules and "
                     <> show (length allModules) <> " source files."))
    Left err -> do
      -- if it fails we load
      --throwError (GeneralError $ "Make failed: " <> show err)
      logInfoN ("Make failed with error: " <> show err)
      logInfoN "Will load externs from file system"
      foundNames <- findAvailableExterns
      let moduleNames = foundNames `intersect` allModuleNames
      let efPaths =
            map (\mn -> outDir </> toS (P.runModuleName mn) </> P.externsFileName)  moduleNames
      efiles <- traverse readExternFile efPaths
      traverse_ insertExterns efiles
      pure (TextResult ("Loaded " <> show (length efiles) <> " modules and "
                    <> show (length allModules) <> " source files."))

  -- pure (TextResult ("Loaded " <> show (length externFiles) <> " modules and "
  --                   <> show (length allModules) <> " source files."))



