-- Tests for the compiler's handling of incremental builds, i.e. the code in
-- Language.PureScript.Make.

module TestMake where

import Prelude hiding (writeFile)

import Language.PureScript qualified as P
import Language.PureScript.CST qualified as CST

import Control.Concurrent (threadDelay)
import Control.Monad (guard, void)
import Control.Exception (tryJust)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (readMVar, newMVar, modifyMVar_)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Text qualified as T
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map qualified as M

import System.FilePath ((</>))
import System.Directory (createDirectory, removeDirectoryRecursive, removeFile, setModificationTime)
import System.IO.Error (isDoesNotExistError)
import System.IO.UTF8 (readUTF8FilesT, writeUTF8FileT)

import Test.Hspec (Spec, before_, it, shouldReturn)
import Debug.Trace (trace)

utcMidnightOnDate :: Integer -> Int -> Int -> UTCTime
utcMidnightOnDate year month day = UTCTime (fromGregorian year month day) (secondsToDiffTime 0)

timestampA, timestampB, timestampC, timestampD :: UTCTime
timestampA = utcMidnightOnDate 2019 1 1
timestampB = utcMidnightOnDate 2019 1 2
timestampC = utcMidnightOnDate 2019 1 3
timestampD = utcMidnightOnDate 2019 1 4

oneSecond :: Int
oneSecond = 10 ^ (6::Int) -- microseconds.

someMs :: Int
someMs = 10 ^ (3::Int) -- microseconds.

spec :: Spec
spec = do
  let sourcesDir = "tests/purs/make"
  let moduleNames = Set.fromList . map P.moduleNameFromString
  let modulePath name = sourcesDir </> (name <> ".purs")
  let foreignJsPath name = sourcesDir </> (name <> ".js")

  before_ (rimraf modulesDir >> rimraf sourcesDir >> createDirectory sourcesDir) $ do
    it "does not recompile if there are no changes" $ do
      let mPath = sourcesDir </> "Module.purs"

      writeFile mPath timestampA "module Module where\nfoo = 0\n"
      compile [mPath] `shouldReturn` moduleNames ["Module"]
      compile [mPath] `shouldReturn` moduleNames []

    it "recompiles if files have changed" $ do
      let mPath = sourcesDir </> "Module.purs"

      writeFile mPath timestampA "module Module where\nfoo = 0\n"
      compile [mPath] `shouldReturn` moduleNames ["Module"]
      writeFile mPath timestampB "module Module where\nfoo = 1\n"
      compile [mPath] `shouldReturn` moduleNames ["Module"]

    it "does not recompile if hashes have not changed" $ do
      let mPath = modulePath "Module"
          moduleContent = "module Module where\nfoo = 0\n"

      writeFile mPath timestampA moduleContent
      compile [mPath] `shouldReturn` moduleNames ["Module"]
      writeFile mPath timestampB moduleContent
      compile [mPath] `shouldReturn` moduleNames []

    it "recompiles if the file path for a module has changed" $ do
      let modulePath1 = sourcesDir </> "Module1.purs"
          modulePath2 = sourcesDir </> "Module2.purs"
          moduleContent = "module Module where\nfoo = 0\n"

      writeFile modulePath1 timestampA moduleContent
      writeFile modulePath2 timestampA moduleContent

      compile [modulePath1] `shouldReturn` moduleNames ["Module"]
      compile [modulePath2] `shouldReturn` moduleNames ["Module"]

    it "recompiles if an FFI file was added" $ do
      let mPath = modulePath "Module"
          mFFIPath = foreignJsPath "Module"
          moduleContent = "module Module where\nfoo = 0\n"

      writeFile mPath timestampA moduleContent
      compile [mPath] `shouldReturn` moduleNames ["Module"]

      writeFile mFFIPath timestampB "export var bar = 1;\n"
      compile [mPath] `shouldReturn` moduleNames ["Module"]

    it "recompiles if an FFI file was removed" $ do
      let mPath = modulePath "Module"
          mFFIPath = foreignJsPath "Module"
          moduleContent = "module Module where\nfoo = 0\n"

      writeFile mPath timestampA moduleContent
      writeFile mFFIPath timestampB "export var bar = 1;\n"
      compile [mPath] `shouldReturn` moduleNames ["Module"]

      removeFile mFFIPath
      compile [mPath] `shouldReturn` moduleNames ["Module"]

    it "recompiles downstream modules when a module is rebuilt and externs changed" $ do
      let mAPath = modulePath "A"
          mBPath = modulePath "B"
          mAContent1 = "module A where\nfoo = 0\n"
          mAContent2 = "module A where\nfoo = 1\nbar = 1\n"
          mBContent = "module B where\nimport A (foo)\nbar = foo\n"

      writeFile mAPath timestampA mAContent1
      writeFile mBPath timestampB mBContent
      compile [mAPath, mBPath] `shouldReturn` moduleNames ["A", "B"]

      writeFile mAPath timestampC mAContent2
      compile [mAPath, mBPath] `shouldReturn` moduleNames ["A", "B"]

    it "only recompiles downstream modules when a module is rebuilt end externs changed" $ do
      let mAPath = modulePath "A"
          mBPath = modulePath "B"
          mCPath = modulePath "C"
          modulePaths = [mAPath, mBPath, mCPath]

          mAContent1 = "module A where\nfoo = 0\n"
          mAContent2 = "module A where\nfoo = 1\nbar = 1\n" -- change externs here
          mBContent = "module B where\nimport A (foo)\nbar = foo\n"
          mCContent = "module C where\nbaz = 3\n"

      writeFile mAPath timestampA mAContent1
      writeFile mBPath timestampB mBContent
      writeFile mCPath timestampC mCContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C"]

      writeFile mAPath timestampD mAContent2
      compile modulePaths `shouldReturn` moduleNames ["A", "B"]

    it "recompiles downstream modules when a reexported module changed" $ do
      let mAPath = modulePath "A"
          mBPath = modulePath "B"
          mCPath = modulePath "C"
          modulePaths = [mAPath, mBPath, mCPath]

          mAContent1 = "module A where\nfoo = 0\n"
          mAContent2 = "module A where\nfoo = 1\nbar = 1\n" -- change externs here
          mBContent = "module B (module E) where\nimport A (foo) as E\n"
          mCContent = "module C where\nimport B as B\nbaz = B.foo\n"

      writeFile mAPath timestampA mAContent1
      writeFile mBPath timestampB mBContent
      writeFile mCPath timestampC mCContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C"]

      writeFile mAPath timestampD mAContent2
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C"]

    it "does not recompile downstream modules when a module is rebuilt but externs have not changed" $ do
      let mAPath = modulePath "A"
          mBPath = modulePath "B"
          mCPath = modulePath "C"
          modulePaths = [mAPath, mBPath, mCPath]

          mAContent1 = "module A where\nfoo = 0\n"
          mAContent2 = "module A (foo) where\nbar = 1\nfoo = 1\n"
          mBContent =
            T.unlines
              [ "module B where"
              , "import A (foo)"
              , "import C (baz)"
              , "bar = foo"
              , "qux = baz"
              ]
          mCContent = "module C where\nbaz = 3\n"

      writeFile mAPath timestampA mAContent1
      writeFile mBPath timestampB mBContent
      writeFile mCPath timestampC mCContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C"]
      --
      writeFile mAPath timestampD mAContent2
      threadDelay oneSecond
      compile modulePaths `shouldReturn` moduleNames ["A"]
      -- compile again to check that it won't try recompile skipped module again
      compile modulePaths `shouldReturn` moduleNames []

    it "does not necessarily recompile modules which were not part of the previous batch" $ do
      let mAPath = modulePath "A"
          mBPath = modulePath "B"
          mCPath = modulePath "C"
          modulePaths = [mAPath, mBPath, mCPath]

          batch1 = [mAPath, mBPath]
          batch2 = [mAPath, mCPath]

          mAContent = "module A where\nfoo = 0\n"
          mBContent = "module B where\nimport A (foo)\nbar = foo\n"
          mCContent = "module C where\nbaz = 3\n"

      writeFile mAPath timestampA mAContent
      writeFile mBPath timestampB mBContent
      writeFile mCPath timestampC mCContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C"]

      compile batch1 `shouldReturn` moduleNames []
      compile batch2 `shouldReturn` moduleNames []

    it "recompiles if a module fails to compile" $ do
      let mPath = sourcesDir </> "Module.purs"
          moduleContent = "module Module where\nfoo :: Int\nfoo = \"not an int\"\n"

      writeFile mPath timestampA moduleContent
      compileAllowingFailures [mPath] `shouldReturn` moduleNames ["Module"]
      compileAllowingFailures [mPath] `shouldReturn` moduleNames ["Module"]

    it "recompiles if docs are requested but not up to date" $ do
      let mPath = sourcesDir </> "Module.purs"

          moduleContent1 = "module Module where\nx :: Int\nx = 1"
          moduleContent2 = moduleContent1 <> "\ny :: Int\ny = 1"

          optsWithDocs = P.defaultOptions { P.optionsCodegenTargets = Set.fromList [P.JS, P.Docs] }
          go opts = compileWithOptions opts [mPath] >>= assertSuccess

      writeFile mPath timestampA moduleContent1
      go optsWithDocs `shouldReturn` moduleNames ["Module"]
      writeFile mPath timestampB moduleContent2
      -- See Note [Sleeping to avoid flaky tests]
      threadDelay oneSecond
      go P.defaultOptions `shouldReturn` moduleNames ["Module"]
      -- Since the existing docs.json is now outdated, the module should be
      -- recompiled.
      go optsWithDocs `shouldReturn` moduleNames ["Module"]

    it "recompiles if CoreFn is requested but not up to date" $ do
      let mPath = sourcesDir </> "Module.purs"
          moduleContent1 = "module Module where\nx :: Int\nx = 1"
          moduleContent2 = moduleContent1 <> "\ny :: Int\ny = 1"
          optsCoreFnOnly = P.defaultOptions { P.optionsCodegenTargets = Set.singleton P.CoreFn }
          go opts = compileWithOptions opts [mPath] >>= assertSuccess

      writeFile mPath timestampA moduleContent1
      go optsCoreFnOnly `shouldReturn` moduleNames ["Module"]
      writeFile mPath timestampB moduleContent2
      -- See Note [Sleeping to avoid flaky tests]
      threadDelay oneSecond
      go P.defaultOptions `shouldReturn` moduleNames ["Module"]
      -- Since the existing CoreFn.json is now outdated, the module should be
      -- recompiled.
      go optsCoreFnOnly `shouldReturn` moduleNames ["Module"]

-- Note [Sleeping to avoid flaky tests]
--
-- One of the things we want to test here is that all requested output files
-- (via the --codegen CLI option) must be up to date if we are to skip
-- recompiling a particular module. Since we check for outdatedness by
-- comparing the timestamp of the output files (eg. CoreFn.json, index.js) to
-- the timestamp of the externs file, this check is susceptible to flakiness
-- if the timestamp resolution is sufficiently coarse. To get around this, we
-- delay for one second.
--
-- Note that most of the compiler behaviour here doesn't depend on file
-- timestamps (instead, content hashes are usually more important) and so
-- sleeping should not be necessary in most of these tests.
--
-- See also discussion on https://github.com/purescript/purescript/pull/4053

rimraf :: FilePath -> IO ()
rimraf =
  void . tryJust (guard . isDoesNotExistError) . removeDirectoryRecursive

-- | Compile a group of modules, returning a set of the modules for which a
-- rebuild was attempted, allowing the caller to set the compiler options and
-- including the make result in the return value.
compileWithOptions ::
  P.Options ->
  [FilePath] ->
  IO (Either P.MultipleErrors [P.ExternsFile], Set P.ModuleName)
compileWithOptions opts input = do
  recompiled <- newMVar Set.empty
  moduleFiles <- readUTF8FilesT input
  (makeResult, _) <- P.runMake opts $ do
    ms <- CST.parseModulesFromFiles id moduleFiles
    let filePathMap = M.fromList $ map (\(fp, pm) -> (P.getModuleName $ CST.resPartial pm, Right fp)) ms
    foreigns <- P.inferForeignModules filePathMap
    let makeActions =
          (P.buildMakeActions modulesDir filePathMap foreigns True)
            { P.progress = \case
                P.CompilingModule mn _ ->
                  liftIO $ modifyMVar_ recompiled (return . Set.insert mn)
                _ -> pure ()
            }
    P.make makeActions (map snd ms)

  recompiledModules <- readMVar recompiled
  pure (makeResult, recompiledModules)

-- | Compile a group of modules using the default options, and including the
-- make result in the return value.
compileWithResult ::
  [FilePath] ->
  IO (Either P.MultipleErrors [P.ExternsFile], Set P.ModuleName)
compileWithResult = compileWithOptions P.defaultOptions

assertSuccess :: (Either P.MultipleErrors a, Set P.ModuleName) -> IO (Set P.ModuleName)
assertSuccess (result, recompiled) =
  case result of
    Left errs ->
      fail (P.prettyPrintMultipleErrors P.defaultPPEOptions errs)
    Right _ ->
      pure recompiled

-- | Compile, returning the set of modules which were rebuilt, and failing if
-- any errors occurred.
compile :: [FilePath] -> IO (Set P.ModuleName)
compile input =
  compileWithResult input >>= assertSuccess

compileAllowingFailures :: [FilePath] -> IO (Set P.ModuleName)
compileAllowingFailures input = fmap snd (compileWithResult input)

writeFile :: FilePath -> UTCTime -> T.Text -> IO ()
writeFile path mtime contents = do
  writeUTF8FileT path contents
  setModificationTime path mtime

-- | Use a different output directory to ensure that we don't get interference
-- from other test results
modulesDir :: FilePath
modulesDir = ".test_modules" </> "make"

