-- Tests for the compiler's handling of incremental builds, i.e. the code in
-- Language.PureScript.Make.

module TestMake (spec) where

import Prelude hiding (writeFile)

import Language.PureScript qualified as P
import Language.PureScript.CST qualified as CST

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Exception (tryJust)
import Control.Monad ( forM_, guard, void, when, join )
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Data.Version (showVersion)

import Paths_purescript qualified as Paths
import System.Directory (createDirectory, createDirectoryIfMissing, getModificationTime, listDirectory, removeDirectoryRecursive, removeFile, setModificationTime)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.IO.UTF8 (readUTF8FileT, readUTF8FilesT, writeUTF8FileT)

import Data.Time (getCurrentTime, addUTCTime)
import Test.Hspec (Spec, before_, fit, it, shouldBe, shouldReturn, shouldSatisfy, xit)
import Data.IORef qualified as Ref

spec :: Spec
spec = do
  -- Before each test.
  before_ cleanUp $ do
    xit "this is just to test compilation env" $ do
      let mPath = sourcesDir </> "Module.purs"

      writeFile mPath timestampA $
        T.unlines
          [ "module Module where"
          , "foo = 0"
          , "bar :: Int -> String"
          , "bar xxx = yyy"
          , "  where yyy = \"xxx_string\""
          ]
      compile [mPath] `shouldReturn` moduleNames ["Module"]
      compile [mPath] `shouldReturn` moduleNames []

    -- RESULTING EXTERNS

    it "returns all externs even when modules not compiled" $ do
      writeModule "A" "module A where foo = 1"
      writeModule "B" "module B where bar = 2"
      ((Right exts1, _), c1) <- compileAll

      c1 `shouldBe` moduleNames ["A", "B"]
      length exts1 `shouldBe` 2

      ((Right exts2, _), c2) <- compileAll
      c2 `shouldBe` moduleNames []

      length exts2 `shouldBe` 2

    it "returns all externs even when modules skipped" $ do
      writeModule "A" "module A where foo = 1"
      writeModule "B" "module B where\nimport A\nbar = foo"
      ((Right exts1, _), c1) <- compileAll

      c1 `shouldBe` moduleNames ["A", "B"]
      length exts1 `shouldBe` 2

      writeModule "A" "module A where foo = 2"

      ((Right exts2, _), c2) <- compileAll
      c2 `shouldBe` moduleNames ["A"]

      length exts2 `shouldBe` 2

    -- PRESERVING WARNINGS

    -- Warnings should are preserved even if modules are not recompiled.
    -- TODO: We may want to check different kind of warnings: (parse, lint, check).
    it "preserves warnings between rebuilds when compilation skipped" $ do
      writeModule "A" $
        T.unlines
          [ "module A (bar) where"
          , -- Unused function.
            "foo :: Int"
          , "foo = 0"
          , "bar :: Int"
          , "bar = 0"
          ]
      ((_, warns), c1) <- compileAll
      c1 `shouldBe` moduleNames ["A"]
      length (P.runMultipleErrors warns) `shouldBe` 1
      --
      ((_, warns2), c2) <- compileAll
      c2 `shouldBe` moduleNames []
      length (P.runMultipleErrors warns2) `shouldBe` 1

    -- CACHE DB

    it "recompiles if cache-db version differs from the current" $ do
      writeModule "Module" "module Module where\nfoo :: Int\nfoo = 1\n"
      compileAll >>= expectCompiled ["Module"]

      -- Replace version with illegal in cache-db file.
      let cacheDbFilePath = P.cacheDbFile outputDir
          versionText ver = "\"version\":\"" <> ver <> "\""

      cacheContent <- readUTF8FileT cacheDbFilePath

      let currentVer = T.pack (showVersion Paths.version)
      let newContent =
            T.replace (versionText currentVer) (versionText "0.0.0") cacheContent

      writeUTF8FileT cacheDbFilePath newContent

      compileAll >>= expectCompiled ["Module"]

    -- COMPILATION SCENARIOS

    it "does not recompile if there are no changes" $ do
      writeModule "Module" "module Module where\nfoo = 0\n"
      compileAll >>= expectCompiled ["Module"]

      compileAll >>= expectCompiled []

    it "recompiles if files have changed" $ do
      writeModule "Module" "module Module where\nfoo = 0\n"
      compileAll >>= expectCompiled ["Module"]

      writeModule "Module" "module Module where\nfoo = 1\n"
      compileAll >>= expectCompiled ["Module"]

    -- If module was re-written with the same content.
    it "does not recompile if hashes have not changed" $ do
      let content = "module Module where\nfoo = 0\n"

      writeModule "Module" content
      compileAll >>= expectCompiled ["Module"]

      writeModule "Module" content
      compileAll >>= expectCompiled []

    -- Allow to rename/move module's source file without recompilation.
    -- This behaviour is changed, previously in was recompiled.
    it "does not recompile if the file path for a module has changed" $ do
      let content = "module Module where\nfoo = 0\n"

      writeModule "Module" content
      compileAll >>= expectCompiled ["Module"]
      deleteModule "Module"

      writeModule "Module2" content
      compileAll >>= expectCompiled []

    it "does not necessarily recompile modules which were not part of the previous batch" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A (foo)\nbar = foo\n"
      writeModule "C" "module C where\nbaz = 3\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      compileSome ["A", "B"] >>= expectCompiled []
      compileSome ["A", "C"] >>= expectCompiled []

    it "recompiles if a module fails to compile" $ do
      let mPath = sourcesDir </> "Module.purs"
          moduleContent = "module Module where\nfoo :: Int\nfoo = \"not an int\"\n"

      writeFile mPath timestampA moduleContent
      compileWithFailure [mPath] `shouldReturn` moduleNames ["Module"]
      compileWithFailure [mPath] `shouldReturn` moduleNames ["Module"]

    it "recompiles if an FFI file was added" $ do
      writeModule "Module" "module Module where\nfoo = 0\n"
      compileAll >>= expectCompiled ["Module"]

      writeForeign "Module" "export var bar = 1;\n"
      compileAll >>= expectCompiled ["Module"]

    it "recompiles if an FFI file was removed" $ do
      writeModule "Module" "module Module where\nfoo = 0\n"
      writeForeign "Module" "export var bar = 1;\n"
      compileAll >>= expectCompiled ["Module"]

      deleteForeign "Module"
      compileAll >>= expectCompiled ["Module"]

    it "recompiles if docs are requested but not up to date" $ do
      let mPath = sourcesDir </> "Module.purs"

          mContent1 = "module Module where\nx :: Int\nx = 1"
          mContent2 = mContent1 <> "\ny :: Int\ny = 1"

          optsWithDocs = P.defaultOptions {P.optionsCodegenTargets = Set.fromList [P.JS, P.Docs]}
          go opts = compileWithOptions opts mempty [mPath] >>= assertSuccess

      writeFile mPath timestampA mContent1
      go optsWithDocs `shouldReturn` moduleNames ["Module"]
      writeFile mPath timestampB mContent2
      -- See Note [Sleeping to avoid flaky tests]
      threadDelay oneSecond
      go P.defaultOptions `shouldReturn` moduleNames ["Module"]
      -- Since the existing docs.json is now outdated, the module should be
      -- recompiled.
      go optsWithDocs `shouldReturn` moduleNames ["Module"]

    it "recompiles if CoreFn is requested but not up to date" $ do
      let mPath = sourcesDir </> "Module.purs"
          mContent1 = "module Module where\nx :: Int\nx = 1"
          mContent2 = mContent1 <> "\ny :: Int\ny = 1"
          optsCoreFnOnly = P.defaultOptions {P.optionsCodegenTargets = Set.singleton P.CoreFn}
          go opts = compileWithOptions opts mempty [mPath] >>= assertSuccess

      writeFile mPath timestampA mContent1
      go optsCoreFnOnly `shouldReturn` moduleNames ["Module"]
      writeFile mPath timestampB mContent2
      -- See Note [Sleeping to avoid flaky tests]
      threadDelay oneSecond
      go P.defaultOptions `shouldReturn` moduleNames ["Module"]
      -- Since the existing CoreFn.json is now outdated, the module should be
      -- recompiled.
      go optsCoreFnOnly `shouldReturn` moduleNames ["Module"]

    it "recompile failed deps in previous compilation" $ do
      writeModule "A" "module A where\nfoo :: Int\nfoo = 0\n"
      writeModule "B" "module B where\nimport A as A\nbar :: Int\nbar = A.foo\n"
      compileAll >>= expectCompiled ["A", "B"]

      threadDelay oneSecond

      writeModule "A" "module A where\nfoo :: Char\nfoo = '0'\n"
      compileAll >>= expectCompiledWithFailure ["A", "B"]

      threadDelay oneSecond

      writeModule "A" "module A where\nfoo :: Char\nfoo = '0'\nfar = 1"
      compileAll >>= expectCompiledWithFailure ["A", "B"]

    it "does not recompile not affected deps after the error fixed" $ do
      writeModule "A" "module A where\nfoo :: Int\nfoo = 0\n"
      writeModule "B" "module B where\nimport A as A\nbar :: Int\nbar = A.foo\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\nfoo :: Char\nfoo = 0\n"
      compileAll >>= expectCompiledWithFailure ["A"]

      writeModule "A" "module A where\nfoo :: Int\nfoo = 0\nzaar = 1"
      compileAll >>= expectCompiled ["A"]

    -- If a module failed to compile, then the error is fixed and there are
    -- effective changes for downstream modules, they should be recompiled.
    it "recompiles affected deps after the error fixed" $ do
      let mAPath = modulePath "A"
          mBPath = modulePath "B"
          mAContent1 = "module A where\nfoo :: Int\nfoo = 0\n"
          mAContent2 = "module A where\nfoo :: Char\nfoo = 0\n"
          mAContent3 = "module A where\nfoo :: Char\nfoo = '0'\n"
          mBContent = "module B where\nimport A as A\nbar :: Int\nbar = A.foo\n"

      writeFile mAPath timestampA mAContent1
      writeFile mBPath timestampB mBContent
      compile [mAPath, mBPath] `shouldReturn` moduleNames ["A", "B"]

      writeFile mAPath timestampC mAContent2
      compileWithFailure [mAPath, mBPath] `shouldReturn` moduleNames ["A"]
      writeFile mAPath timestampD mAContent3
      compileWithFailure [mAPath, mBPath] `shouldReturn` moduleNames ["A", "B"]

    -- REBUILD CUT OFF: rebuilds only modules that are affected by changes.

    -- RebuildReason:: LaterDependency

    it "recompiles downstream in case of later dependency" $ do
      -- C and B depends on A.
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A\nbar = 1\nbaz = foo\n"
      writeModule "C" "module C where\nimport A\nimport B\nqux = bar\nthud = foo"

      compileAll >>= expectCompiled ["A", "B", "C"]

      threadDelay oneSecond

      writeModule "A" "module A where\nfoo = '1'\n"
      _ <- compileOne "A"

      compileAll >>= expectCompiled ["B", "C"]

    -- Later dependency should only require compilation of direct downstream modules.
    it "recompiles only direct deps in case of later dependency" $ do
      -- Only B depends on A. C not effected.
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A\nbar = 1\nbaz = foo\n"
      writeModule "C" "module C where\nimport B\nqux = baz"

      compileAll >>= expectCompiled ["A", "B", "C"]

      threadDelay oneSecond

      writeModule "A" "module A where\nfoo = 1\n"
      _ <- compileOne "A"

      compileAll >>= expectCompiled ["B"]

      -- Check timestamp for C is modified.
      tsB <- getOutputTimestamp "B"
      tsC <- getOutputTimestamp "C"
      tsC `shouldSatisfy` (<=) tsB

    it "recompiles downstream in case of later dependency with transitive change" $ do
      -- C and B depends on A. A effects C.
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A\nbar = 1\nbaz = foo\n"
      writeModule "C" "module C where\nimport B\nqux = baz"

      compileAll >>= expectCompiled ["A", "B", "C"]

      threadDelay oneSecond

      -- Change foo's type (effect on C).
      writeModule "A" "module A where\nfoo = '1'\n"
      _ <- compileOne "A"

      compileAll >>= expectCompiled ["B", "C"]

    -- RebuildReason: UpstreamRef

    it "recompiles downstream modules when module's externs change (Updated ref)" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A as A\nbar = A.foo\n"

      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\nfoo = '1'\n" -- change foo type
      compileAll >>= expectCompiled ["A", "B"]

    it "skips downstream rebuild when externs has not changed" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A as A\nbar = A.foo\n"

      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\n\nfoo = 1\n" -- no type change
      compileAll >>= expectCompiled ["A"]

    it "skips downstream rebuild when externs changed but do not affect (Added ref)" $ do
      writeModule "A" "module A where\nfoo = 0"
      writeModule "B" "module B where\nimport A as A\nbar = A.foo\n"

      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\n\nfoo = 0\n\nbaz = 1"

      compileAll >>= expectCompiled ["A"]

    it "recompiles downstream rebuild when externs add ref which cause conflict" $ do
      writeModule "A" "module A where\nfoo = 0"
      writeModule "B" "module B where\nbar = '1'\n"
      writeModule "C" "module C where\nimport A\nimport B\ncar = bar\n"

      compileAll >>= expectCompiled ["A", "B", "C"]

      -- Add `bar` in A  which is present in B too.
      writeModule "A" "module A where\nfoo = 0\nbar = 1"

      compileAll >>= expectCompiledWithFailure ["A", "C"]

    it "recompiles downstream rebuild when added ref causes ScopeShadowing" $ do
      writeModule "A" "module A where\nfoo = 0"
      writeModule "B" "module B where\nbar = '1'\n"
      writeModule "C" "module C where\nimport A\nimport B (bar)\ncar = bar\n"

      compileAll >>= expectCompiled ["A", "B", "C"]

      -- Add `bar` in A  which is present in B too. Will cause ScopeShadowing in C.
      writeModule "A" "module A where\nfoo = 0\nbar = 1"

      compileAll >>= expectCompiled ["A", "C"]

    -- Type arguments changes.

    it "renaming type arguments doesn't cause downstream rebuild" $ do
      let typ = "data Foo a = Foo\n"
      let fn = "foo :: forall a. Int -> Foo a\nfoo _ = Foo\n"

      writeModule "A" $ "module A where\n" <> typ <> fn
      writeModule "B" "module B where\nimport A as A\nbar = A.foo\n"

      compileAll >>= expectCompiled ["A", "B"]

      let typ2 = "data Foo x = Foo\n"
      let fn2 = "foo :: forall y. Int -> Foo y\nfoo _ = Foo\n"
      writeModule "A" $ "module A where\n" <> typ2 <> fn2 <> "x = 1\n"

      compileAll >>= expectCompiled ["A"]

    it "changing order of type arguments causes downstream rebuild" $ do
      let fn = "foo :: forall a b. a -> b -> Int\nfoo _ _ = 1\n"

      writeModule "A" $ "module A where\n" <> fn
      writeModule "B" "module B where\nimport A as A\nbar = A.foo\n"

      compileAll >>= expectCompiled ["A", "B"]

      let fn2 = "foo :: forall b a. a -> b -> Int\nfoo _ _ = 1\n"
      writeModule "A" $ "module A where\n" <> fn2

      compileAll >>= expectCompiled ["A", "B"]

    it "renaming data type arguments doesn't cause downstream rebuild" $ do
      let typ = "data Baz a b = Foo a | Bar b\n"

      writeModule "A" $ "module A where\n" <> typ
      writeModule "B" "module B where\nimport A\nbar = (Foo 1 :: Baz Int String)\n"

      compileAll >>= expectCompiled ["A", "B"]

      -- Rename a <-> b, this doesn't change types.
      let typ2 = "data Baz b a = Foo b | Bar a\n"
      writeModule "A" $ "module A where\n" <> typ2

      compileAll >>= expectCompiled ["A"]

    it "changing order of data type arguments causes downstream rebuild" $ do
      let typ = "data Baz a b = Foo a | Bar b\n"

      writeModule "A" $ "module A where\n" <> typ
      writeModule "B" "module B where\nimport A\nbar = (Foo 1 :: Baz Int String)\n"

      compileAll >>= expectCompiled ["A", "B"]

      -- Changing a <-> b order (on the left) will cause change in forall
      -- signature of constructors.
      let typ2 = "data Baz b a = Foo a | Bar b\n"
      writeModule "A" $ "module A where\n" <> typ2

      compileAll >>= expectCompiledWithFailure ["A", "B"]

    -- This is because adding/removing a constructor may affect cases
    -- statements that do not use it explicitly.
    -- Though this potentially could be optimized while searching though the module.
    it "adding type constructor causes downstream rebuild if it uses (another) constructor" $ do
      let typ = "data Baz a b = Foo a | Bar b\n"

      writeModule "A" $ "module A where\n" <> typ
      writeModule "B" "module B where\nimport A\nbar = (Foo 1 :: Baz Int String)\n"

      compileAll >>= expectCompiled ["A", "B"]

      let typ2 = "data Baz b a = Foo b | Bar a | Car\n"
      writeModule "A" $ "module A where\n" <> typ2

      -- As B uses constructor adding constructor affects
      compileAll >>= expectCompiled ["A", "B"]

    -- If dependency uses only a type without constructors, it should not care
    -- about right side changes.
    it "adding type constructor doesn't cause downstream rebuild if it uses only the type" $ do
      let typ = "data Baz a b = Foo a | Bar b\n"

      writeModule "A" $ "module A where\n" <> typ
      writeModule "B" "module B where\nimport A\nbar (x :: Baz String Int) = 1"

      compileAll >>= expectCompiled ["A", "B"]

      let typ2 = "data Baz b a = Foo b | Bar a | Car\n"
      writeModule "A" $ "module A where\n" <> typ2

      compileAll >>= expectCompiled ["A"]

    -- it "typeclass deal" $ do
    --   let typ = "data Baz = Foo | Bar\n"
    --   let cls = "class Cls a where toInt :: a -> Int\n"
    --   let inst = "instance Cls Baz where toInt _ = 1"

    --   writeModule "A" $ "module A where\n" <> cls <> typ <> inst

    --   writeModule "B" "module B where\nimport A\nbar = toInt Bar"

    --   compileAll >>= expectCompiled ["A", "B"]
    --   ex1 <- getCompiledExterns "A" <$> compileAll

    --   writeModule "A" $ "module A where\n\n\n" <> cls <> typ <> inst

    --   compileAll >>= expectCompiled ["A"]
    --   ex2 <- getCompiledExterns "A" <$> compileAll

    --   print $ diffExterns ex2 ex1 []



    -- Reexports: original ref is changed.
    test3
      xit
      "recompiles downstream when a reexported ref changed"
      ( "module A where\nfoo = 0\n"
      , "module A where\nfoo = '1'\nbar = 1\n" -- change externs here
      , "module B (module E) where\nimport A (foo) as E\n"
      , "module C where\nimport B as B\nbaz = B.foo\n"
      )
      ["A", "B", "C"]

    it "recompiles downstream when a reexported ref changed" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B (module E) where\nimport A (foo) as E\n"
      writeModule "C" "module C where\nimport B as B\nbaz = B.foo\n"

      compileAll >>= expectCompiled ["A", "B", "C"]

      -- Change externs here.
      writeModule "A" "module A where\nfoo = '1'\nbar = 1\n"

      compileAll >>= expectCompiled ["A", "B", "C"]

    -- Reexports: original ref is changed. Ref is imported but not used.
    test3
      it
      "does not recompile downstream when a reexported ref changed and the ref is imported but not used"
      ( "module A where\nfoo = 0\n"
      , "module A where\nfoo = '1'\nbar = 1\n" -- change externs here
      , "module B (module E) where\nimport A as E\n"
      , -- Import but not use.
        "module C where\nimport B (foo)\nx = 1\n"
      )
      ["A", "B"]

    -- Reexports: original export is removed from module.
    testWithFailure3
      it
      "recompiles downstream when a reexported ref removed"
      ( "module A where\nfoo = 0\n"
      , "module A where\nbar = 1\n" -- change externs here
      , "module B (module E) where\nimport A as E\n"
      , "module C where\nimport B as B\nbaz = B.foo\n"
      )
      ["A", "B", "C"]

    -- Reexports: ref is removed from reexporting module.
    testWithFailure3
      it
      "recompiles downstream when a reexported ref removed (from reexported)"
      ( "module B (module E) where\nimport A (foo) as E\n"
      , "module B where\nimport A (foo) as E\n"
      , "module A where\nfoo = 0\n"
      , "module C where\nimport B as B\nbaz = B.foo\n"
      )
      ["B", "C"]

    -- Reexports: ref is imported but not used. Reexport ref is removed from
    -- reexporting module.
    testWithFailure3
      it
      "recompiles downstream when a reexported ref removed (imported but not used)"
      ( "module B (module E) where\nimport A (foo) as E\n"
      , "module B where\nimport A (foo) as E\n"
      , "module A where\nfoo = 0\n"
      , -- Import but not use.
        "module C where\nimport B (foo) as B\nx=1\n"
      )
      ["B", "C"]

    -- Reexports: original ref Removed. Ref is imported but not used.
    testWithFailure3
      it
      "recompiles downstream when a reexported ref removed in original"
      ( "module A where\nfoo = 0\n"
      , "module A where\nbar = 1\n" -- change externs here
      , "module B (module E) where\nimport A as E\n"
      , -- Import but not use.
        "module C where\nimport B (foo)\nx = 1\n"
      )
      ["A", "B", "C"]

    -- Imports.
    testWithFailure2
      it
      "recompiles downstream when removed reference found in imports"
      ( "module A where\nfoo = 0\n"
      , "module A where\nfoo2 = 1\n"
      , "module B where\nimport A (foo)\nbar = 1"
      )
      ["A", "B"]

    test2
      it
      "does not recompiles downstream when removed reference is not used"
      ( "module A where\nfoo = 0\n"
      , "module A where\nfoo2 = 1\n"
      , "module B where\nimport A\nbar = 1"
      )
      ["A"]

    -- We need to ensure that it finds refs everywhere inside a module.
    -- Usage: Inlined type.
    testWithFailure2
      it
      "recompiles downstream when found changed inlined type"
      ( "module A where\ntype T = Int\n"
      , "module A where\ntype T = String\n"
      , "module B where\nimport A\nx = (1 :: T)"
      )
      ["A", "B"]

    -- Transitive change: module A changes, module B depends on A and module C
    -- depends on B are both recompiled.
    test3
      it
      "recompiles downstream due to transitive change"
      ( "module A where\nfoo = 0\n"
      , "module A where\nfoo = '1'\n"
      , "module B where\nimport A (foo)\nbar = qux\nqux = foo"
      , "module C where\nimport B (bar)\nbaz = bar\n"
      )
      ["A", "B", "C"]

    test3
      it
      "does not recompile downstream if no transitive change"
      ( "module A where\nfoo = 0\n"
      , "module A where\nfoo = '1'\n"
      , "module B where\nimport A (foo)\nbar = 1\nqux = foo"
      , "module C where\nimport B (bar)\nbaz = bar\n"
      )
      ["A", "B"]

    -- Non effective change does not cause downstream rebuild.
    test2
      it
      "does not recompile downstream if unused type changed"
      ( "module A where\ntype SynA = Int\ntype SynA2 = Int"
      , "module A where\ntype SynA = String\ntype SynA2 = Int"
      , "module B where\nimport A as A\ntype SynB = A.SynA2"
      )
      ["A"]

    -- Type synonym change.
    recompile2
      it
      "type synonym changed"
      ( "module A where\ntype SynA = Int\n"
      , "module A where\ntype SynA = String\n"
      , "module B where\nimport A as A\ntype SynB = Array A.SynA\n"
      )

    -- Type synonym change in value.
    recompile2
      it
      "type synonym changed in value"
      ( "module A where\ntype SynA = Int\n"
      , "module A where\ntype SynA = String\n"
      , "module B where\nimport A as A\nvalue = ([] :: Array A.SynA)\n"
      )

    -- Type synonym change in pattern.
    recompile2
      it
      "type synonym changed in pattern"
      ( "module A where\ntype SynA = Int\n"
      , "module A where\ntype SynA = String\n"
      , "module B where\nimport A as A\nfn = \\(_ :: Array A.SynA) -> 0\n"
      )

    -- Type synonym indirect change.
    recompile2
      it
      "type synonym dependency changed"
      ( "module A where\ntype SynA = Int\ntype SynA2 = SynA\n"
      , "module A where\ntype SynA = String\ntype SynA2 = SynA\n"
      , "module B where\nimport A as A\ntype SynB = Array A.SynA2\n"
      )

    -- Data type: parameter added.
    recompile2
      it
      "data type changed (parameter added)"
      ( "module A where\ndata T = A Int | B Int\n"
      , "module A where\ndata T a = A Int | B a\n"
      , "module B where\nimport A (T)\ntype B = T"
      )

    -- Data type: constructor added.
    recompile2
      it
      "data type changed (constructor added)"
      ( "module A where\ndata T = A | B\n"
      , "module A where\ndata T = A | B | C\n"
      , "module B where\nimport A (T(B))\nb = B"
      )

    -- Data type: constructor indirectly changed.
    recompile2
      it
      "data type constructor dependency changed"
      ( "module A where\ntype SynA = Int\ndata AB = A SynA | B Int\n"
      , "module A where\ntype SynA = String\ndata AB = A SynA | B Int\n"
      , "module B where\nimport A (AB(..))\nb = A"
      )

    -- Data type: constructor changed but not used.
    noRecompile2
      it
      "data type constructor changed, but not used"
      ( "module A where\ntype SynA = Int\ndata AB = A SynA | B Int\n"
      , "module A where\ntype SynA = String\ndata AB = A SynA | B Int\n"
      , -- use type and other constructor
        "module B where\nimport A (AB(..))\ntype B = AB\nb = B"
      )

    -- Data type: constructor added, but not imported.
    noRecompile2
      it
      "data type constructor added, but ctors not imported"
      ( "module A where\ntype SynA = Int\ndata AB = A SynA | B Int\n"
      , "module A where\ntype SynA = String\ndata AB = A SynA | B Int | C\n"
      , -- use just type
        "module B where\nimport A (AB)\ntype B = AB\n"
      )

    -- Data type: constructor added, but not used.
    noRecompile2
      it
      "data type constructor added, but ctors not imported"
      ( "module A where\ntype SynA = Int\ndata AB = A SynA | B Int\n"
      , "module A where\ntype SynA = String\ndata AB = A SynA | B Int | C\n"
      , -- use type
        "module B where\nimport A (AB(..))\ntype B = AB\n"
      )

    -- Data type: constructor added, and constructors are used in the downstream
    -- module (this may be need when there is a case statement without wildcard,
    -- but we don't analyze the usage that deep).
    recompile2
      it
      "data type constructor added and ctors are used"
      ( "module A where\ntype SynA = Int\ndata AB = A SynA | B Int\n"
      , "module A where\ntype SynA = String\ndata AB = A SynA | B Int | C\n"
      , -- use type and other constructor
        "module B where\nimport A (AB(..))\ntype B = AB\nb = B\n"
      )

    -- Value operator change.
    recompile2
      it
      "value op changed"
      ( "module A where\ndata T a = T Int a\ninfixl 2 T as :+:\n"
      , "module A where\ndata T a = T Int a\ninfixl 3 T as :+:\n"
      , "module B where\nimport A\nt = 1 :+: \"1\" "
      )

    -- Value operator indirect change.
    recompile2
      it
      "value op dependency changed"
      ( "module A where\ndata T a = T a String\ninfixl 2 T as :+:\n"
      , "module A where\ndata T a = T Int a\ninfixl 2 T as :+:\n"
      , "module B where\nimport A\nt = 1 :+: \"1\" "
      )

    -- Type operator change.
    recompile2
      it
      "type op changed"
      ( "module A where\ndata T a b = T a b\ninfixl 2 type T as :+:\n"
      , "module A where\ndata T a b = T a b\ninfixl 3 type T as :+:\n"
      , "module B where\nimport A\nfn :: Int :+: String -> Int\nfn _ = 1"
      )

    -- Type operator indirect change.
    -- fit "recompiles downstream type op dependency changed" $ do
    --   writeModule "A" "module A where\ndata T a b = T a b\ninfixl 2 type T as :+:\n"
    --   writeModule "B" "module B where\nimport A\nfn :: Int :+: String -> Int\nfn _ = 1"

    --   compileAll >>= expectCompiled ["A", "B"]

    --   writeModule "A" "module A where\ndata T b a = T a b\ninfixl 2 type T as :+:\n"

    --   compileAll >>= expectCompiled ["A", "B"]

    -- Type classes changed. Downstream uses type class in signature.
    recompile2
      it
      "type class changed"
      ( "module A where\nclass Cls a where m1 :: a -> Int\n"
      , "module A where\nclass Cls a where m1 :: a -> Char\n"
      , T.unlines
          [ "module B where"
          , "import A as A"
          , "fn :: forall a. A.Cls a => a -> Int"
          , "fn _ = 1"
          ]
      )

    -- Type classes changed. Downstream uses only its member.
    recompile2
      it
      "type class changed (member affected)"
      ( "module A where\nclass Cls a where m1 :: a -> Int\n"
      , "module A where\nclass Cls a where m1 :: a -> Char\n"
      , T.unlines
          [ "module B where"
          , "import A as A"
          , "fn x = A.m1 x"
          ]
      )

    -- Type class instance added.
    recompile2
      it
      "type class instance added"
      ( "module A where\nclass Cls a where m1 :: a -> Int\n"
      , "module A where\nclass Cls a where m1 :: a -> Int\ninstance Cls Int where m1 _ = 1"
      , T.unlines
          [ "module B where"
          , "import A as A"
          , "fn :: forall a. A.Cls a => a -> Int"
          , "fn _ = 1"
          ]
      )

    -- Type class instance removed.
    recompileWithFailure2
      it
      "type class instance removed"
      ( "module A where\nclass Cls a where m1 :: a -> Int\ninstance Cls Int where m1 _ = 1"
      , "module A where\nclass Cls a where m1 :: a -> Int\n"
      , T.unlines
          [ "module B where"
          , "import A (m1)"
          , "x = m1 1"
          ]
      )

    -- Type class instance added for a type. We need to recompile downstream
    -- modules that use this type, because it can be effected (even if it
    -- doesn't use type class as we do not analyze this).
    test3
      it
      "recompiles downstream if instance added for a type"
      ( "module B where\nimport A\nnewtype T = T Int\n"
      , "module B where\nimport A\nnewtype T = T Int\ninstance Cls T where m1 _ = 1"
      , "module A where\nclass Cls a where m1 :: a -> Int\n"
      , T.unlines
          [ "module C where"
          , "import B"
          , "t = T 1"
          ]
      )
      ["B", "C"]

    -- Type class instance removed for a type.
    testWithFailure3
      it
      "recompiles downstream if type class instance removed for a type"
      ( "module B where\nimport A\nnewtype T = T Int\ninstance Cls T where m1 _ = 1"
      , "module B where\nimport A\nnewtype T = T Int\n"
      , "module A where\nclass Cls a where m1 :: a -> Int\n"
      , T.unlines
          [ "module C where"
          , "import A"
          , "import B"
          , "i :: Int"
          , "i = m1 (T 1)"
          ]
      )
      ["B", "C"]

    -- Type class instance added for the type and type class in another module
    -- it self is modified. We don't need to recompile downstream modules that
    -- depend only on type (if they use type class they will be recompiled).
    testN
      it
      "does not recompile downstream if an instance added for the type and type class changed"
      [
        ( "A"
        , "module A where\nclass Cls a where m1 :: a -> Char\n"
        , Just "module A where\nclass Cls a where m1 :: a -> Int\n"
        )
      ,
        ( "B"
        , "module B where\nimport A\nnewtype T = T Int\n"
        , Just "module B where\nimport A\nnewtype T = T Int\ninstance Cls T where m1 _ = 1"
        )
      , ("C", "module C where\nimport B\ntype C = T", Nothing)
      ]
      compile
      ["A", "B"]

    -- Type in foreign import value.
    recompile2
      it
      "type synonym changed in foreign import"
      ( "module A where\ntype SynA = Int\n"
      , "module A where\ntype SynA = String\n"
      , "module B where\nimport A as A\nforeign import a :: A.SynA\n"
      )

    -- Foreign import value's type.
    recompile2
      it
      "foreign import value's type changed"
      ( "module A where\nforeign import a :: Int\n"
      , "module A where\nforeign import a :: String\n"
      , "module B where\nimport A as A\nb = A.a\n"
      )

    -- Foreign data type.
    recompileWithFailure2
      it
      "foreign import type's type changed"
      ( "module A where\nforeign import data True :: Boolean\n"
      , "module A where\nforeign import data True :: Int\n"
      , "module B where\nimport A\ndata Proxy (t :: Boolean) = Proxy\np0 = (Proxy :: Proxy True)"
      )

    -- Kind signatures.
    recompileWithFailure2
      it
      "type in kind changed"
      ( "module A where\ntype Id a = a\n"
      , "module A where\ntype Id = Int\n"
      , T.unlines
          [ "module B where"
          , "import A (Id)"
          , "data Proxy :: forall (k :: Id Type). k -> (Id Type)"
          , "data Proxy a  = Proxy"
          ]
      )

    recompileWithFailure2
      it
      "type in kind"
      ( "module A where\ntype Id a = a\n"
      , "module A where\ntype Id = Int\n"
      , T.unlines
          [ "module B where"
          , "import A (Id)"
          , "class Test (a :: Id Type)"
          ]
      )

    -- Not clear what this tests was for:
    -- it "does not recompile downstream when a module is rebuilt but externs have not changed" $ do
    --   let mAPath = modulePath "A"
    --       mBPath = modulePath "B"
    --       mCPath = modulePath "C"
    --       modulePaths = [mAPath, mBPath, mCPath]

    --       mAContent1 = "module A where\nfoo = 0\n"
    --       mAContent2 = "module A (foo) where\nbar = 1\nfoo = 1\n"
    --       mBContent =
    --         T.unlines
    --           [ "module B where"
    --           , "import A (foo)"
    --           , "import C (baz)"
    --           , "bar = foo"
    --           , "qux = baz"
    --           ]
    --       mCContent = "module C where\nbaz = 3\n"

    --   writeFile mAPath timestampA mAContent1
    --   writeFile mBPath timestampB mBContent
    --   writeFile mCPath timestampC mCContent
    --   compile modulePaths `shouldReturn` moduleNames ["A", "B", "C"]
    --   --
    --   writeFile mAPath timestampD mAContent2
    --   threadDelay oneSecond
    --   compile modulePaths `shouldReturn` moduleNames ["A"]
    --   -- compile again to check that it won't try recompile skipped module again
    --   compile modulePaths `shouldReturn` moduleNames []

    -- ASDF
    it "asdf does not recompile if there are no changes" $ do
      let modulePath = sourcesDir </> "Module.purs"

      writeFile modulePath timestampA "module Module where\nfoo = 0\n"
      compile [modulePath] `shouldReturn` moduleNames ["Module"]
      compile [modulePath] `shouldReturn` moduleNames []

    it "asdf recompiles if files have changed" $ do
      let modulePath = sourcesDir </> "Module.purs"

      writeFile modulePath timestampA "module Module where\nfoo = 0\n"
      compile [modulePath] `shouldReturn` moduleNames ["Module"]
      writeFile modulePath timestampB "module Module where\nfoo = 1\n"
      compile [modulePath] `shouldReturn` moduleNames ["Module"]

    it "asdf does not recompile if hashes have not changed" $ do
      let modulePath = sourcesDir </> "Module.purs"
          moduleContent = "module Module where\nfoo = 0\n"

      writeFile modulePath timestampA moduleContent
      compile [modulePath] `shouldReturn` moduleNames ["Module"]
      writeFile modulePath timestampB moduleContent
      compile [modulePath] `shouldReturn` moduleNames []

    -- ASDF (This is from alternative PR) -- should be removed

    -- Modified data type constructor
    it "asdf recompiles downstream modules when a data type constructor changes" $ do
      let moduleAPath = sourcesDir </> "A.purs"
          moduleBPath = sourcesDir </> "B.purs"
          moduleAContent1 = "module A where\ndata Foo = Foo | Foo10\n"
          moduleAContent2 = "module A where\ndata Foo = Foo | Foo11\n"
          moduleBContent = "module B where\nimport A (Foo(..))\nbar = Foo\n"

      writeFile moduleAPath timestampA moduleAContent1
      writeFile moduleBPath timestampB moduleBContent
      compile [moduleAPath, moduleBPath] `shouldReturn` moduleNames ["A", "B"]

      writeFile moduleAPath timestampC moduleAContent2
      compile [moduleAPath, moduleBPath] `shouldReturn` moduleNames ["A", "B"]

    it "asdf recompiles direct dependents but not transitive dependents when a data type constructor changes" $ do
      let moduleAPath = sourcesDir </> "A.purs"
          moduleBPath = sourcesDir </> "B.purs"
          moduleCPath = sourcesDir </> "C.purs"
          modulePaths = [moduleAPath, moduleBPath, moduleCPath]
          moduleAContent1 = "module A where\ndata Foo = Foo | Foo20\n"
          moduleAContent2 = "module A where\ndata Foo = Foo | Foo21\n"
          moduleBContent = "module B where\nimport A (Foo(..))\nbar = Foo\n"
          moduleCContent = "module C where\nbaz = 23\n"

      writeFile moduleAPath timestampA moduleAContent1
      writeFile moduleBPath timestampB moduleBContent
      writeFile moduleCPath timestampC moduleCContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C"]

      writeFile moduleAPath timestampD moduleAContent2
      compile modulePaths `shouldReturn` moduleNames ["A", "B"]

    it "asdf only recompiles direct dependents even if they don't import the constructors" $ do
      -- NOTE[drathier]: while constructors can still be exported in spirit via
      -- a Generic instance, all type class instances live next to the data type
      -- or the type class definition, and we don't inline it, so it will have
      -- been updated already
      let moduleAPath = sourcesDir </> "A.purs"
          moduleBPath = sourcesDir </> "B.purs"
          moduleCPath = sourcesDir </> "C.purs"
          modulePaths = [moduleAPath, moduleBPath, moduleCPath]
          moduleAContent1 = "module A where\ndata Foo = Foo | Foo20\n"
          moduleAContent2 = "module A where\ndata Foo = Foo | Foo21\n"
          moduleBContent = "module B where\nimport A (Foo(..))\nbar = Foo\n"
          moduleCContent = "module C where\nimport A (Foo)\nbaz :: Foo -> Foo\nbaz a = a\n"

      writeFile moduleAPath timestampA moduleAContent1
      writeFile moduleBPath timestampB moduleBContent
      writeFile moduleCPath timestampC moduleCContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C"]

      writeFile moduleAPath timestampD moduleAContent2
      compile modulePaths `shouldReturn` moduleNames ["A", "B"]

    it "asdf does not recompile anything when no source files changed" $ do
      let moduleAPath = sourcesDir </> "A.purs"
          moduleBPath = sourcesDir </> "B.purs"
          moduleCPath = sourcesDir </> "C.purs"
          modulePaths = [moduleAPath, moduleBPath, moduleCPath]
          batch1 = [moduleAPath, moduleBPath]
          batch2 = [moduleAPath, moduleCPath]
          moduleAContent = "module A where\nfoo = 0\n"
          moduleBContent = "module B where\nimport A (foo)\nbar = foo\n"
          moduleCContent = "module C where\nbaz = 3\n"

      writeFile moduleAPath timestampA moduleAContent
      writeFile moduleBPath timestampB moduleBContent
      writeFile moduleCPath timestampC moduleCContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C"]

      compile batch1 `shouldReturn` moduleNames []
      compile batch2 `shouldReturn` moduleNames []

    it "asdf only recompiles one module if it only differs in whitespace" $ do
      -- not sure if it should recompile anything here, but it does, so this will let us know if anything changes
      let moduleAPath = sourcesDir </> "A.purs"
          moduleBPath = sourcesDir </> "B.purs"
          moduleCPath = sourcesDir </> "C.purs"
          modulePaths = [moduleAPath, moduleBPath, moduleCPath]
          moduleAContent1 = "module A where\nfoo = 0\n"
          moduleAContent2 = "module A where \nfoo = 0\n"
          moduleBContent = "module B where\nimport A (foo)\nbar = foo\n"
          moduleCContent = "module C where\nbaz = 3\n"

      writeFile moduleAPath timestampA moduleAContent1
      writeFile moduleBPath timestampB moduleBContent
      writeFile moduleCPath timestampC moduleCContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C"]

      -- no changes when rebuilding
      compile modulePaths `shouldReturn` moduleNames []

      -- and again with changes only in whitespace
      writeFile moduleAPath timestampD moduleAContent2
      compile modulePaths `shouldReturn` moduleNames ["A"]

    it "asdf only recompiles nothing if it only differs in timestamps" $ do
      -- not sure if it should recompile anything here, but it does, so this will let us know if anything changes
      let moduleAPath = sourcesDir </> "A.purs"
          moduleBPath = sourcesDir </> "B.purs"
          moduleCPath = sourcesDir </> "C.purs"
          modulePaths = [moduleAPath, moduleBPath, moduleCPath]
          moduleAContent = "module A where\nfoo = 0\n"
          moduleBContent = "module B where\nimport A (foo)\nbar = foo\n"
          moduleCContent = "module C where\nbaz = 3\n"

      writeFile moduleAPath timestampA moduleAContent
      writeFile moduleBPath timestampA moduleBContent
      writeFile moduleCPath timestampA moduleCContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C"]

      -- no changes when rebuilding
      compile modulePaths `shouldReturn` moduleNames []

      -- and again with changes only in timestamp
      writeFile moduleAPath timestampB moduleAContent
      writeFile moduleBPath timestampC moduleBContent
      writeFile moduleCPath timestampD moduleCContent
      compile modulePaths `shouldReturn` moduleNames []

    -- More complicated caching rules; transitive type aliases
    it "asdf transitively tracks the underlying type of type aliases, with local type alias" $ do
      let moduleAPath = sourcesDir </> "A.purs"
          moduleBPath = sourcesDir </> "B.purs"
          moduleCPath = sourcesDir </> "C.purs"
          modulePaths = [moduleAPath, moduleBPath, moduleCPath]
          moduleAContent1 = "module A where\ntype TA = Int\n"
          moduleAContent2 = "module A where\ntype TA = String\n"
          moduleBContent = "module B where\nimport A\ntype TB = TA\n"
          moduleCContent = "module C where\nimport B\ntype TC = TB\nthingy :: TC\nthingy = 42\n"

      writeFile moduleAPath timestampA moduleAContent1
      writeFile moduleBPath timestampB moduleBContent
      writeFile moduleCPath timestampC moduleCContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C"]

      -- no changes when rebuilding
      compile modulePaths `shouldReturn` moduleNames []
      -- type aliases need to be tracked transitively; otherwise the public api of B doesn't change after this compile, and the build succeeds, even though C.thingy = 42 now has a type annotation that says it's a String
      writeFile moduleAPath timestampD moduleAContent2

      (Left _errors, recompiledModules) <- lmap fst <$> compileWithResult mempty modulePaths
      _ <- recompiledModules `shouldBe` moduleNames ["A", "B", "C"]
      pure ()

    it "asdf transitively tracks the underlying type of type aliases, without local type alias" $ do
      let moduleAPath = sourcesDir </> "A.purs"
          moduleBPath = sourcesDir </> "B.purs"
          moduleCPath = sourcesDir </> "C.purs"
          modulePaths = [moduleAPath, moduleBPath, moduleCPath]
          moduleAContent1 = "module A where\ntype TA = Int\n"
          moduleAContent2 = "module A where\ntype TA = String\n"
          moduleBContent = "module B where\nimport A\ntype TB = TA\n"
          moduleCContent = "module C where\nimport B\nthingy :: TB\nthingy = 42\n"

      writeFile moduleAPath timestampA moduleAContent1
      writeFile moduleBPath timestampB moduleBContent
      writeFile moduleCPath timestampC moduleCContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C"]

      -- no changes when rebuilding
      compile modulePaths `shouldReturn` moduleNames []
      -- type aliases need to be tracked transitively; otherwise the public api of B doesn't change after this compile, and the build succeeds, even though C.thingy = 42 now has a type annotation that says it's a String
      writeFile moduleAPath timestampD moduleAContent2

      (Left _errors, recompiledModules) <- lmap fst <$> compileWithResult mempty modulePaths
      recompiledModules `shouldBe` moduleNames ["A", "B", "C"]
      pure ()

    it "asdf should not consider private type aliases to be part of the public api of a module" $ do
      let moduleAPath = sourcesDir </> "A.purs"
          moduleBPath = sourcesDir </> "B.purs"
          modulePaths = [moduleAPath, moduleBPath]
          moduleAContent1 = "module A (TAPublic) where\ntype TAPublic = Int\ntype TAPrivate = Int\n"
          moduleAContent2 = "module A (TAPublic) where\ntype TAPublic = Int\ntype TAPrivate = String\n"
          moduleBContent = "module B where\nimport A\nthingy :: TAPublic\nthingy = 42\n"

      writeFile moduleAPath timestampA moduleAContent1
      writeFile moduleBPath timestampB moduleBContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B"]

      -- no changes when rebuilding
      compile modulePaths `shouldReturn` moduleNames []
      -- type aliases need to be tracked transitively; otherwise the public api of B doesn't change after this compile, and the build succeeds, even though C.thingy = 42 now has a type annotation that says it's a String
      writeFile moduleAPath timestampD moduleAContent2
      compile modulePaths `shouldReturn` moduleNames ["A"]
      pure ()

    -- More complicated caching rules; transitive type classes
    it "asdf transitively tracks the underlying type class instance implementations of type classes; instance in same module as type class definition" $ do
      -- first, type class without type arguments
      let moduleAPath = sourcesDir </> "A.purs"
          moduleBPath = sourcesDir </> "B.purs"
          moduleCPath = sourcesDir </> "C.purs"
          modulePaths = [moduleAPath, moduleBPath, moduleCPath]
          moduleAContent1 = "module A where\nclass TC a where\n  tc :: a\ninstance TC Int where\n  tc = 42\n"
          moduleAContent2 = "module A where\nclass TC a where\n  tc :: a\ninstance TC Int where\n  tc = 55\n"
          moduleBContent = "module B where\nimport A\nthingy = tc\n"
          moduleCContent = "module C where\nimport B\nasdf = thingy :: Int\n"

      writeFile moduleAPath timestampA moduleAContent1
      writeFile moduleBPath timestampB moduleBContent
      writeFile moduleCPath timestampC moduleCContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C"]

      -- no changes when rebuilding
      compile modulePaths `shouldReturn` moduleNames []

      -- type class defs need to be tracked transitively; otherwise the public api of B doesn't change after this compile, since C is now implicitly depending on A, via the type class implementation for TC Int, via B
      writeFile moduleAPath timestampD moduleAContent2
      -- [drathier]: I thought we'd have to recompile B and C here, since they use the modified type class instance, but since it references the type class instance as a top-level function in that other module, we apparently only have to recompile A
      -- NOTE[drathiner]: I'm quite worried that even if this works right now, it might not work later. What I'd actually want is to eval some constants, or pattern match the corefn, but that would give a ton of incorrect test failures.
      -- compile modulePaths `shouldReturn` moduleNames ["A", "B", "C"]
      compile modulePaths `shouldReturn` moduleNames ["A"]
      -- no changes when rebuilding
      compile modulePaths `shouldReturn` moduleNames []

    it "asdf transitively tracks the underlying type class instance implementations of type classes; instance in same module as data type definition" $ do
      -- first, type class without type arguments
      let moduleAPath = sourcesDir </> "A.purs"
          moduleBPath = sourcesDir </> "B.purs"
          moduleCPath = sourcesDir </> "C.purs"
          moduleDPath = sourcesDir </> "D.purs"
          modulePaths = [moduleAPath, moduleBPath, moduleCPath, moduleDPath]
          -- diamond shape
          moduleAContent = "module A where\nclass TC a where\n  tc :: a\n"
          moduleBContent1 = "module B where\nimport A\ndata BT = BT Int\ninstance TC BT where\n  tc = BT (42)\n"
          moduleBContent2 = "module B where\nimport A\ndata BT = BT Int\ninstance TC BT where\n  tc = BT (55)\n"
          moduleCContent = "module C where\nimport A\nthingy = tc\n"
          moduleDContent = "module D where\nimport B\nimport C\nasdf = thingy :: BT\n"

      -- psm(D).asdf
      -- {:bT, 42}
      -- moduleBContent1 to moduleBContent2
      -- psm(D).asdf
      -- {:bT, 55}
      -- instance is referred to as in D b@ps:tCBT()

      writeFile moduleAPath timestampA moduleAContent
      writeFile moduleBPath timestampB moduleBContent1
      writeFile moduleCPath timestampC moduleCContent
      writeFile moduleDPath timestampD moduleDContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C", "D"]

      -- no changes when rebuilding
      compile modulePaths `shouldReturn` moduleNames []

      -- type class defs need to be tracked transitively; otherwise the public api of B doesn't change after this compile, since C is now implicitly depending on A, via the type class implementation for TC Int, via B
      writeFile moduleBPath timestampD moduleBContent2
      -- [drathier]: I thought we'd have to recompile D too, but since it references the type class instance as a top-level function in that other module, we don't have to recompile D
      -- compile modulePaths `shouldReturn` moduleNames ["B", "D"]
      compile modulePaths `shouldReturn` moduleNames ["B"]
      -- no changes when rebuilding
      compile modulePaths `shouldReturn` moduleNames []

    it "asdf tracks type class super classes across module boundaries" $ do
      -- first, type class without type arguments
      let moduleAPath = sourcesDir </> "A.purs"
          moduleBPath = sourcesDir </> "B.purs"
          moduleCPath = sourcesDir </> "C.purs"
          moduleDPath = sourcesDir </> "D.purs"
          modulePaths = [moduleAPath, moduleBPath, moduleCPath, moduleDPath]
          -- diamond shape
          moduleAContent1 = "module A where\nclass TCA a <= TC a where\n  tc :: a\nclass TCA a\nclass TCB a\nclass TCC a\n"
          moduleAContent12 = "module A where\nclass (TCA a, TCB a) <= TC a where\n  tc :: a\nclass TCA a\nclass TCB a\nclass TCC a\n"
          moduleAContent2 = "module A where\nclass TCB a <= TC a where\n  tc :: a\nclass TCA a\nclass TCB a\nclass TCC a\n"
          moduleAContent3 = "module A where\nclass TCC a <= TC a where\n  tc :: a\nclass TCA a\nclass TCB a\nclass TCC a\n"
          moduleBContent = "module B where\nimport A\ndata BT = BT Int\ninstance TC BT where\n  tc = BT (55)\ninstance TCA BT\ninstance TCB BT\n"
          moduleCContent = "module C where\nimport A\nthingy = tc\n"
          moduleDContent = "module D where\nimport B\nimport C\nasdf = thingy :: BT\n"

      writeFile moduleAPath timestampA moduleAContent1
      writeFile moduleBPath timestampB moduleBContent
      writeFile moduleCPath timestampC moduleCContent
      writeFile moduleDPath timestampD moduleDContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C", "D"]

      -- no changes when rebuilding
      compile modulePaths `shouldReturn` moduleNames []

      -- change super class of type class
      writeFile moduleAPath timestampB moduleAContent2
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C", "D"]
      -- no changes when rebuilding
      compile modulePaths `shouldReturn` moduleNames []

      -- add second super class of type class
      writeFile moduleAPath timestampC moduleAContent12
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C", "D"]
      -- no changes when rebuilding
      compile modulePaths `shouldReturn` moduleNames []

      -- change super class of type class to one without instance for BT
      writeFile moduleAPath timestampD moduleAContent3
      -- should fail because of missing type class instance TCC BT
      (Left _errors, recompiledModules) <- lmap fst <$> compileWithResult mempty modulePaths
      recompiledModules `shouldBe` moduleNames ["A", "B", "C"]

      -- change back to something that should work
      writeFile moduleAPath timestampE moduleAContent1
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C", "D"]

      -- no changes when rebuilding
      compile modulePaths `shouldReturn` moduleNames []

    it "asdf changing an unexported declaration doesn't trigger downstream recompiles" $ do
      -- first, type class without type arguments
      let moduleAPath = sourcesDir </> "A.purs"
          moduleBPath = sourcesDir </> "B.purs"
          modulePaths = [moduleAPath, moduleBPath]
          -- diamond shape
          moduleAContent1 = "module A where\nthingy = 1\n"
          moduleAContent2 = "module A where\nthingy = 2\n"
          moduleBContent = "module B where\nimport A\nasdf = thingy\n"

      writeFile moduleAPath timestampA moduleAContent1
      writeFile moduleBPath timestampB moduleBContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B"]

      writeFile moduleAPath timestampD moduleAContent2
      compile modulePaths `shouldReturn` moduleNames ["A"]
      -- no changes when rebuilding
      compile modulePaths `shouldReturn` moduleNames []

    it "asdf adding a new declaration doesn't have to trigger downstream recompiles" $ do
      -- first, type class without type arguments
      let moduleAPath = sourcesDir </> "A.purs"
          moduleBPath = sourcesDir </> "B.purs"
          modulePaths = [moduleAPath, moduleBPath]
          -- diamond shape
          moduleAContent1 = "module A where\nthingy = 1\n"
          moduleAContent2 = "module A where\nthingy = 2\nqwer = 234\n"
          moduleBContent = "module B where\nimport A\nasdf = thingy\n"

      writeFile moduleAPath timestampA moduleAContent1
      writeFile moduleBPath timestampB moduleBContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B"]

      writeFile moduleAPath timestampD moduleAContent2
      compile modulePaths `shouldReturn` moduleNames ["A"]

      -- no changes when rebuilding
      compile modulePaths `shouldReturn` moduleNames []

    it "asdf adding a new data declaration doesn't have to trigger downstream recompiles" $ do
      -- first, type class without type arguments
      let moduleAPath = sourcesDir </> "A.purs"
          moduleBPath = sourcesDir </> "B.purs"
          modulePaths = [moduleAPath, moduleBPath]
          -- diamond shape
          moduleAContent1 = "module A where\nthingy = 1\n"
          moduleAContent2 = "module A where\nthingy = 2\ndata MyType = MyTypeCtor\n"
          moduleBContent = "module B where\nimport A (thingy)\nasdf = thingy\n"

      writeFile moduleAPath timestampA moduleAContent1
      writeFile moduleBPath timestampB moduleBContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B"]

      writeFile moduleAPath timestampD moduleAContent2
      compile modulePaths `shouldReturn` moduleNames ["A"]

      -- no changes when rebuilding
      compile modulePaths `shouldReturn` moduleNames []
  where
    -- END ASDF

    sourcesDir = "tests/purs/make"
    moduleNames = Set.fromList . map P.moduleNameFromString
    modulePath name = sourcesDir </> (T.unpack name <> ".purs")
    foreignJsPath name = sourcesDir </> (T.unpack name <> ".js")

    cleanUp = do
      rimraf outputDir >> rimraf sourcesDir >> createDirectory sourcesDir

    writeModule mn content = do
      ts <- getCurrentTime
      writeFile (modulePath mn) ts content

    deleteModule mn = do
      removeFile (modulePath mn)

    writeForeign mn content = do
      ts <- getCurrentTime
      writeFile (foreignJsPath mn) ts content

    getOutputTimestamp mn =
      getModificationTime (modulePath mn)

    deleteForeign mn = do
      removeFile (foreignJsPath mn)

    listModulePaths =
      fmap ((</>) sourcesDir)
        <$> filter (T.isSuffixOf ".purs" . T.pack)
        <$> listDirectory sourcesDir

    compileAll = do
      sources <- listModulePaths
      compileWithResult mempty sources

    compileSome mns = do
      let sources = modulePath <$> mns
      compileWithResult mempty sources

    compileOne mn = do
      compileWithResult mempty [modulePath mn]

    expectCompiled mns r = do
      compiled <- assertSuccess r
      compiled `shouldBe` moduleNames mns

    expectCompiledWithFailure mns r = do
      compiled <- assertFailure r
      compiled `shouldBe` moduleNames mns

    getCompiledExterns mn r =
      case res of
        Right exts | Just ex <- find ((==) (P.moduleNameFromString mn) . P.efModuleName) exts -> ex
        _ -> error $ "No such externs for module " <> T.unpack mn
      where
        ((res, _), _) = r

    -- Perv Test helpers, should be replaced.
    testN itFn name modules compileFn res =
      itFn name $ do
        let names = map (\(mn, _, _) -> mn) modules
        let paths = map modulePath names
        let timestamp = utcMidnightOnDate 2019 1

        forM_ (zip [0 ..] modules) $ \(idx, (mn, content, _)) -> do
          writeFile (modulePath mn) (timestamp idx) content
          -- Write a fake foreign module to bypass compiler's check.
          when (T.isInfixOf "\nforeign import" content) $
            writeFile (foreignJsPath mn) (timestamp idx) content

        compile paths `shouldReturn` moduleNames names

        forM_ (zip [length modules ..] modules) $ \(idx, (mn, _, mbContent)) -> do
          maybe (pure ()) (writeFile (modulePath mn) (timestamp idx)) mbContent

        compileFn paths `shouldReturn` moduleNames res

    test2 fn name (mAContent1, mAContent2, mBContent) res =
      testN
        fn
        name
        [ ("A", mAContent1, Just mAContent2)
        , ("B", mBContent, Nothing)
        ]
        compile
        res

    testWithFailure2 fn name (mAContent1, mAContent2, mBContent) res =
      testN
        fn
        name
        [ ("A", mAContent1, Just mAContent2)
        , ("B", mBContent, Nothing)
        ]
        compileWithFailure
        res

    test3 fn name (mAContent1, mAContent2, mBContent, mCContent) res =
      testN
        fn
        name
        [ ("A", mAContent1, Just mAContent2)
        , ("B", mBContent, Nothing)
        , ("C", mCContent, Nothing)
        ]
        compile
        res

    testWithFailure3 fn name (mAContent1, mAContent2, mBContent, mCContent) res =
      testN
        fn
        name
        [ ("A", mAContent1, Just mAContent2)
        , ("B", mBContent, Nothing)
        , ("C", mCContent, Nothing)
        ]
        compileWithFailure
        res

    recompile2 fn name ms =
      test2 fn ("recompiles downstream when " <> name) ms ["A", "B"]

    recompileWithFailure2 fn name ms =
      testWithFailure2 fn ("recompiles downstream when " <> name) ms ["A", "B"]

    noRecompile2 fn name ms =
      test2 fn ("does not recompile downstream when " <> name) ms ["A"]

utcMidnightOnDate :: Integer -> Int -> Int -> UTCTime
utcMidnightOnDate year month day = UTCTime (fromGregorian year month day) (secondsToDiffTime 0)

timestampA, timestampB, timestampC, timestampD :: UTCTime
timestampA = utcMidnightOnDate 2019 1 1
timestampB = utcMidnightOnDate 2019 1 2
timestampC = utcMidnightOnDate 2019 1 3
timestampD = utcMidnightOnDate 2019 1 4
timestampE = utcMidnightOnDate 2019 1 5

oneSecond :: Int
oneSecond = 10 ^ (5 :: Int) -- microseconds.

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

type CompileResult = (Either P.MultipleErrors [P.ExternsFile], P.MultipleErrors)
type CompiledModule = (P.ModuleName, P.RebuildReason)
-- | Compile a group of modules, returning a set of the modules for which a
-- rebuild was attempted, allowing the caller to set the compiler options and
-- including the make result in the return value.
compileWithOptions' ::
  P.Options ->
  M.Map P.ModuleName P.RebuildPolicy ->
  [FilePath] ->
  IO (CompileResult, Set CompiledModule)
compileWithOptions' opts policyMap input = do
  recompiled <- newMVar Set.empty
  moduleFiles <- readUTF8FilesT input

  _ <- createDirectoryIfMissing True outputDir

  outTimeRef <- getCurrentTime >>= Ref.newIORef
  let getNextOutTs = Ref.atomicModifyIORef' outTimeRef (join (,) . addUTCTime 0.1)

  (makeResult, warnings) <- P.runMake opts $ do
    ms <- CST.parseModulesFromFiles id moduleFiles

    let filePathMap =
          M.union (Left <$> policyMap) $
            M.fromList (map (\(fp, pm) -> (P.getModuleName $ CST.resPartial pm, Right fp)) ms)

    foreigns <- P.inferForeignModules filePathMap

    let logFile = outputDir </> "compile.log"
    let cleanLog = False
    logProgress <- P.progressWithFile logFile cleanLog
    let actions = P.buildMakeActions outputDir filePathMap foreigns True
    let updateTimestamp mn = liftIO getNextOutTs >>= P.updateOutputTimestamp actions mn . Just
    let makeActions =
          (P.buildMakeActions outputDir filePathMap foreigns True)
            { P.progress =
                (*>) <$> logProgress <*> \case
                  P.CompilingModule mn _ reason ->
                    liftIO $ modifyMVar_ recompiled (return . Set.insert (mn, reason))
                  _ -> pure ()
            -- , P.codegen = \ann d ext wrn -> do
            --     P.codegen actions ann d ext wrn
            --     lift $ void $ updateTimestamp (CF.moduleName ann)
            -- , P.updateOutputTimestamp = \mn _ -> updateTimestamp mn
            }
    P.make makeActions (map snd ms)

  recompiledModules <- readMVar recompiled
  pure ((makeResult, warnings), recompiledModules)

compileWithOptions ::
  P.Options ->
  M.Map P.ModuleName P.RebuildPolicy ->
  [FilePath] ->
  IO (CompileResult, Set P.ModuleName)
compileWithOptions opts policyMap =
  fmap (fmap (Set.map fst)) . compileWithOptions' opts policyMap


-- | Compile a group of modules using the default options, and including the
-- make result in the return value.
compileWithResult ::
  M.Map P.ModuleName P.RebuildPolicy ->
  [FilePath] ->
  IO (CompileResult, Set P.ModuleName)
compileWithResult = compileWithOptions P.defaultOptions

assertSuccess :: (CompileResult, a) -> IO a
assertSuccess ((result, _), recompiled) =
  case result of
    Left errs ->
      fail (P.prettyPrintMultipleErrors P.defaultPPEOptions errs)
    Right _ ->
      pure recompiled

assertFailure :: (CompileResult, a) -> IO a
assertFailure ((result, _), recompiled) =
  case result of
    Left _ ->
      pure recompiled
    Right _ ->
      fail "should compile with errors"

lmap :: (a -> c) -> (a, b) -> (c, b)
lmap f (a, b) = (f a, b)

-- | Compile, returning the set of modules which were rebuilt, and failing if
-- any errors occurred.
compile :: [FilePath] -> IO (Set P.ModuleName)
compile input =
    --fmap (fmap fst) . compile'
    compileWithResult mempty input >>= assertSuccess

-- | Compile, returning the set of modules which were rebuilt, and failing if
-- any errors occurred.
-- compile' :: [FilePath] -> IO (Set CompiledModule)
-- compile' input =
--   compileWithResult mempty input >>= assertSuccess


-- compileWithFailure' :: [FilePath] -> IO (Set CompiledModule)
-- compileWithFailure' input =
--   compileWithResult mempty input >>= assertFailure

compileWithFailure :: [FilePath] -> IO (Set P.ModuleName)
compileWithFailure input =
    --fmap (fmap fst) . compileWithFailure'
    compileWithResult mempty input >>= assertFailure

writeFile :: FilePath -> UTCTime -> T.Text -> IO ()
writeFile path mtime contents = do
  writeUTF8FileT path contents
  setModificationTime path mtime

-- | Use a different output directory to ensure that we don't get interference
-- from other test results
outputDir :: FilePath
outputDir = ".test_modules" </> "make"
