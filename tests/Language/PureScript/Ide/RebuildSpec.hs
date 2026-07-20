module Language.PureScript.Ide.RebuildSpec (spec) where

import Protolude

import Data.Set qualified as Set
import Data.Text qualified as Text
import Language.PureScript qualified as P
import Language.PureScript.Errors qualified as Errors
import Language.PureScript.AST.SourcePos (spanName)
import Language.PureScript.Errors qualified as Errors
import Language.PureScript.Ide.Command (Command(..), RebuildFile(..))
import Language.PureScript.Ide.Completion (defaultCompletionOptions)
import Language.PureScript.Ide.Matcher (flexMatcher)
import Language.PureScript.Ide.Types (Completion(..), Success(..), emptyIdeState, )
import Language.PureScript.Ide.Types as Ide
import Language.PureScript.Ide.Test qualified as Test


import System.FilePath ((</>))
import System.Directory (doesFileExist, doesDirectoryExist, removePathForcibly, getModificationTime)
import Test.Hspec as H
import Language.PureScript (MakeActions(readCacheDb))
import Control.Exception (throw)
import Language.PureScript.Ide.State qualified as S
import GHC.Base (error)
import Language.PureScript.Ide.Error (IdeError(..))
import System.IO.UTF8 (readUTF8FileT)


defaultTarget :: Set P.CodegenTarget
defaultTarget = Set.singleton P.JS

load :: [Text] -> Command
load = LoadSync . map Test.mn

rebuildAll :: Set P.CodegenTarget  -> Command
rebuildAll = RebuildAll

rebuildMany :: [(FilePath, Text)] -> Set P.CodegenTarget  -> Command
rebuildMany files = RebuildMany $ uncurry RebuildFile <$> files

rebuildOne :: (FilePath, Text) -> Set P.CodegenTarget  -> Command
rebuildOne files = RebuildOne $ uncurry RebuildFile files

reset :: Command
reset = Reset

rebuild :: FilePath -> Command
rebuild fp = Rebuild ("src" </> fp) Nothing defaultTarget

rebuildSync :: FilePath -> Command
rebuildSync fp = RebuildSync ("src" </> fp) Nothing defaultTarget

spec :: Spec
spec = do

  describe "Rebuilding modules" $ before_ cleanUp $ do
    it (pfx "rebuildAll rebuilds with no codegen") $ do

      ([r], _) <- runIde
        [ rebuildAll noCodegen ]

      expectNoErrors r
      expectNoWarnings r
      expectCompiledAll r

      checkFileDoesNotExist "output/cache-db.json"

    it (pfx "rebuildAll rebuilds with codegen") $ do

      ([r], _) <- runIde
        [ rebuildAll withCodegen ]

      expectNoErrors r
      expectNoWarnings r
      expectCompiledAll r

      checkFileExists "output/cache-db.json"

    it (pfx "rebuildAll on the next run does no codegen") $ do

      ([_], _) <- runIde
        [ rebuildAll withCodegen ]

      let outputPath = moduleOutputPath "A"
      outputPath `expectNoTimestampChangesAfter` do
        ([r], _) <- runIde
          [ rebuildAll withCodegen ]

        expectNoErrors r
        expectCompiledAll r

    it (pfx "rebuildAll on existing compilation without codegen") $ do
      compileProject

      "output/cache-db.json" `expectNoTimestampChangesAfter` do
        ([r], _) <- runIde
            [ rebuildAll withCodegen ]

        expectCompiledAll r

    it (pfx "rebuildAll doesn't cause project recompile") $ do
      compileProject

      ([r], _) <- runIde
            [ rebuildAll withCodegen ]

      expectCompiledAll r

      let outputPath = moduleOutputPath "A"
      expectNoTimestampChangesAfter outputPath $ do
        compileProject

    it (pfx "rebuildAll with added files") $ do

      ([_], st) <- runIde
        [ rebuildAll withCodegen ]

      writeModule "D" "module D where dull = 0"

      checkFileDoesNotExist (moduleOutputPath "D")

      ([r], st') <- runIdeWithState st
          [ rebuildAll withCodegen ]

      checkFileExists (moduleOutputPath "D")
      fpC <- moduleAbsPath "C"

      ([r2], _) <- runIdeWithState st'
          [ rebuildOne (fpC, "module C where\nimport D\ncool = dull") noCodegen ]

      expectNoErrors r2
      -- expectCompiledAll r

    it (pfx "rebuildAll returns warnings") $ do
      writeModule "A" (moduleAText  <> "\nx = 1")

      ([r], _) <- runIde
        [ rebuildAll withCodegen ]

      expectCompiledAll r
      expectWarningsFor r "A"

    -- If a module has ModuleNotFound error we do not want it all dependencies.
    it (pfx "rebuildAll rebuilds modules beside modules with ModuleNotFound error") $ do
      -- compileProject

      writeModule "A" "module A where\nimport X\nx = X.x"

      ([r], _) <- runIde
        [ rebuildAll withCodegen ]
      print r

      expectCompiledCount r 2
      expectErrorsFor r "A"
      -- B is not compiled because depends on A.
      expectNoErrorsFor r "C"

    -- When running rebuildAll it should return results for all modules,
    -- including prebuilt.
    it (pfx "rebuildAll returns warnings for already compiled") $ do
      writeModule "A" (moduleAText  <> "\nx = 1")
      compileProject

      ([r], _) <- runIde
        [ rebuildAll withCodegen ]

      expectWarningsFor r "A"

    -- Should not return previous version warnings.
    it (pfx "rebuildAll return errors for modules that have prior warnings") $ do
      writeModule "A" (moduleAText  <> "\nx = 1")
      compileProject
      writeModule "A" (moduleAText  <> "\nx :: String\nx = 1")

      ([r], _) <- runIde
        [ rebuildAll withCodegen ]

      expectNoWarnings r
      expectErrorsFor r "A"

    it (pfx "RebuildAll returns error if FFI module missing") $ do
      compileProject
      writeModule "A" "module A where\nforeign import x :: Int"
      fpA <- moduleAbsPath "A"

      ([r], _) <- runIde
        [ rebuildAll withCodegen
        ]

      -- No hint in the error for moduleName.
      r `expectErrorsForFp` fpA

    -- Test for RebuildMany ----
    it (pfx "rebuildMany with codegen writes changed output to disk") $ do
      compileProject

      let updatedCText = "module C where\nzero :: Int\nzero = 1"
      --writeModule "C" moduleCText

      tA <- getModuleOutputTimestamp "A"
      tC <- getModuleOutputTimestamp "C"

      fpC <- moduleAbsPath "C"
      ([_], _) <- runIde
        [ rebuildMany [(fpC, updatedCText)] withCodegen
        ]

      tA' <- getModuleOutputTimestamp "A"
      tC' <- getModuleOutputTimestamp "C"


      tA' `shouldBe` tA
      tC' `shouldNotBe` tC

    it (pfx "Fails: rebuildMany with codegen writes changed output to disk") $ do
      compileProject

      let updatedAText = "module A where\nfoo :: Int\nfoo = 42\nx=1"
      let updatedBText = "module B where\nimport A\nbar = 1"

      tA <- readModuleOutputText "A"
      tB <- readModuleOutputText "B"

      fpA <- moduleAbsPath "A"
      fpB <- moduleAbsPath "B"
      (_, _) <- runIde
        [ rebuildAll withCodegen
        ---, rebuildOne (fpB, updatedBText) noCodegen
        , rebuildMany [(fpB, updatedBText)] noCodegen
        --, rebuildOne (fpA, updatedAText) noCodegen
        --, rebuildMany [(fpA, updatedAText)] noCodegen
        , rebuildMany [(fpA, updatedAText), (fpB, updatedBText)] withCodegen
        ]

      tA' <- readModuleOutputText "A"
      tB' <- readModuleOutputText "B"

      --expectNoErrors r
      --print tA
      tA' `shouldNotBe` tA
      tB' `shouldNotBe` tB

    -- fit (pfx "Some") $ do

    --   let updatedAText = "module A where\nfoo :: Char\nfoo = '4'"
    --   let fixedBText = "module B where\nimport A\nbar = (foo :: Char)"


    --   fpA <- moduleAbsPath "A"
    --   fpB <- moduleAbsPath "B"

    --   ([_, _, r1, _, rFix, r2, r3], _) <- runIde
    --     [ rebuildAll withCodegen
    --     , rebuildOne (fpA, updatedAText) noCodegen
    --     , rebuildMany [(fpA, updatedAText)] noCodegen
    --     , rebuildOne (fpB, fixedBText) noCodegen
    --     , rebuildMany [(fpB, fixedBText)] noCodegen
    --     -- Some change in A.
    --     , rebuildOne (fpA, updatedAText <> "x") noCodegen
    --     , rebuildMany [(fpA, updatedAText <> "x")] noCodegen
    --     -- -- Some change in B.
    --     -- , rebuildOne (fpB, fixedBText <> " ") noCodegen
    --     -- , rebuildMany [(fpB, fixedBText <> " ")] noCodegen
    --     ]

    --   r1 `expectErrorsFor` "B"
    --   expectNoErrors rFix
    --   expectNoErrors r2
    --   expectNoErrors r3



    -- replace module case and foreigns
    it (pfx "Module names") $ do
      Test.deleteSrcFolder

      writeModule "A" "module Aa where\nforeign import x :: Int"
      writeForeignModule "A" "export x = 1"

      let fixedText = "module A where\nforeign import x :: Int"

      fpA <- moduleAbsPath "A"
      ([_, r, r1], _) <- runIde
        [ rebuildAll withCodegen
        , rebuildOne (fpA, fixedText) noCodegen
        , rebuildMany [(fpA, fixedText)] withCodegen
        ]

      expectNoErrors r
      expectNoErrors r1


    it (pfx "other failure") $ do

      fpA <- moduleAbsPath "A"
      fpB <- moduleAbsPath "B"

      ([_, _, r1, r2], _) <- runIde
        [ rebuildAll withCodegen
        , rebuildOne (fpA, changeDepType moduleAText) noCodegen
        , rebuildMany [(fpA, changeDepType moduleAText)] withCodegen
        -- Update A externs, to cause B rebuild
        , rebuildMany [(fpA, changeDepType moduleAText <> "\nzoo=3")] noCodegen
        --     docsCodegen
        ]

      r1 `expectErrorsFor` "B"
      r2 `expectErrorsFor` "B"

    it (pfx "Prim fail") $ do
      Test.deleteSrcFolder
      writeModule "F" "module F where\nx :: Int\nx = 1"
      --compileProject
      --P.JS, P.JSSourceMap, P.CoreFn,
      let docsCodegen = Set.fromList [P.Docs]
      fpF <- moduleAbsPath "F"
      ([_, _, r], _) <- runIde
        [ rebuildAll docsCodegen
        , rebuildOne (fpF, "module F where\nx :: Int\nx = 1\n--") docsCodegen
        , rebuildMany [(fpF, "module F where\nx :: Int\nx = 1\n--")]
            docsCodegen
        ]

      expectNoErrors r

    it (pfx "rebuildMany after rebuildAll case of parser error on module") $ do
      compileProject
      -- Introduce parser error.
      fpA <- moduleAbsPath "A"
      fpB <- moduleAbsPath "B"
      fpC <- moduleAbsPath "C"

      writeModule "A" ("X" <> moduleAText)

      ([r, r1], _) <- runIde
        [ rebuildAll withCodegen
        -- Fix the error in A.
        , rebuildMany [(fpA, moduleAText)] noCodegen
        ]

      expectCompiled r [fpB, fpC]

      -- Should return parser error for A.
      expectErrorsForFp r fpA
      expectErrorsFor r "B"

      expectCompiled r1 [fpA, fpB]
      expectNoErrors r1


    it (pfx "rebuildMany with codegen doesn't cause project recompile") $ do
      fpA <- moduleAbsPath "A"
      let originalText  = moduleAText
      let updatedText = originalText & changeDepValue

      ([r, r2, r3], _) <- runIde
        [ rebuildAll withCodegen
        , rebuildMany [(fpA, updatedText)] withCodegen
        -- revert cache-db hash back
        , rebuildMany [(fpA, originalText)] withCodegen
        ]

      expectCompiledAll r
      -- [fpA] -> [fpA, fpB]
      -- expectCompiled r2 [fpA]
      -- expectCompiled r3 [fpA]

      moduleOutputPath "A" `expectNoTimestampChangesAfter` compileProject

    xit (pfx "Some inconsistent flow") $ do
      let mAText = "module A where\nfoo = 42\nfar = 43"
      let mBText = "module B where\nimport A\nbar = (foo :: Int)"
      let mCText = "module C where\nimport A\nimport B\ncar = (bar :: Int)\nzar = (far :: Int)"

      writeModule "A" mAText
      writeModule "B" mBText
      writeModule "C" mCText

      fpA <- moduleAbsPath "A"
      fpB <- moduleAbsPath "B"
      fpC <- moduleAbsPath "C"

      --let updatedText = originalText & changeDepValue
      let updateFoo = Text.replace "42" "\"42\""
      let updateFar = Text.replace "43" "\"43\""
      ([r, r1, r2, r3], _) <- runIde
        [ rebuildAll withCodegen
        , rebuildMany [(fpA, mAText & updateFoo)] noCodegen
        , rebuildMany [(fpA, mAText & updateFoo & updateFar)] noCodegen
        , rebuildMany [(fpA, mAText)] noCodegen
        ]

      expectCompiledAll r
      expectNoErrors r

      expectCompiled r1 [fpA, fpB]
      expectErrorsFor r1 "B"

      expectCompiled r2 [fpA, fpC]
      expectErrorsFor r2 "C"

      expectCompiled r3 [fpA, fpB]
      expectNoErrors r3
      --expectErrorsFor r2 "C"
      -- expectCompiled r2 [fpA]
      -- expectCompiled r3 [fpA]

      --moduleOutputPath "A" `expectNoTimestampChangesAfter` compileProject

    -- Module sent to RebuildOne will not be recompiled if then sent to
    -- RebuildMany if externs do not change.
    it (pfx "RebuildOne without externs change does recompile module in RebuildMany") $ do
      compileProject

      let text = moduleAText
      fpA <- moduleAbsPath "A"
      --fpB <- moduleAbsPath "B"

      ([_, r1, r2], _) <- runIde
        [ rebuildAll withCodegen
        , rebuildOne (fpA, changeDepValue text)  noCodegen
        , rebuildMany [(fpA, changeDepValue text)]  noCodegen
        ]

      expectCompiled r1 [fpA]
      expectNoErrors r1

      expectCompiled r2 []
      expectNoErrors r2

    it (pfx "TODO: rebuildOne with error rebuildMany") $ do
      -- compileProject

      let text = moduleAText
      fpA <- moduleAbsPath "A"
      fpB <- moduleAbsPath "B"

      ([_, r1, r2, _, r4], _) <- runIde
        [ rebuildAll withCodegen
        , rebuildOne (fpA, changeDepType text)  noCodegen
        , rebuildMany [(fpA, changeDepType text)]  noCodegen
        -- Revert A back.
        , rebuildOne (fpA, text)  noCodegen
        , rebuildMany [(fpA, text)]  noCodegen
        ]

      expectCompiled r1 [fpA]
      expectNoErrors r1

      expectCompiled r2 [fpA, fpB]
      expectErrorsFor r2 "B"

      expectCompiled r4 [fpA, fpB]
      expectNoErrors r4

    -- TODO: Make appropriate name.
    xit (pfx "RebuildMany with codegen A and C does not causes B to be rebuild (no A laterDep issue)") $ do

      fpA <- moduleAbsPath "A"
      fpC <- moduleAbsPath "C"

      ([_, r1, r2], _) <- runIde
        [ rebuildAll withCodegen
        , rebuildMany [(fpA, moduleAText <> " ")]  withCodegen
        , rebuildMany [(fpC, moduleCText <> " ")]  noCodegen
        ]

      expectCompiled r1 [fpA]

      expectCompiled r2 [fpC]

    it (pfx "RebuildMany recompiles deps in case of re-added ref") $ do

      fpA <- moduleAbsPath "A"
      fpB <- moduleAbsPath "B"

      ([_, r1, r2], _) <- runIde
        [ rebuildAll withCodegen
        , rebuildMany [(fpA, "module A where")]  withCodegen
        , rebuildMany [(fpA, moduleAText)]  noCodegen
        ]

      expectCompiled r1 [fpA, fpB]
      expectErrorsFor r1 "B"

      expectCompiled r2 [fpA, fpB]
      expectNoErrorsFor r2 "B"




    it (pfx "RebuildOne promotes added ref to available externs") $ do
      compileProject

      let updatedAText = moduleAText <> "\nnew = 1"
      fpA <- moduleAbsPath "A"

      ([_, _, r2, c1], _) <- runIde
        [ rebuildAll withCodegen
        , rebuildOne (fpA, updatedAText)  noCodegen
        , rebuildMany [(fpA, updatedAText)]  noCodegen
        , Complete [] (flexMatcher "ne") (Just (Test.mn "B")) defaultCompletionOptions
        ]

      -- A compiled because externs changed in RebuildOne.
      -- Again don't check b/c added skipped compiled
      --expectCompiled r2 [fpA]

      c1 `returnsCompletionIdents` ["new"]




    it (pfx "RebuildOne returns error if FFI module missing ") $ do
      compileProject
      fpA <- moduleAbsPath "A"

      ([_, r], _) <- runIde
        [ rebuildAll withCodegen
        , rebuildOne (fpA, "module A where\nforeign import x :: Int")  noCodegen

        ]

      -- No hint in the error for moduleName.
      r `expectErrorsForFp` fpA

    it (pfx "RebuildOne and rebuildMany") $ do
      compileProject
      fpA <- moduleAbsPath "A"
      fpB <- moduleAbsPath "B"


      let withErrorBText = "module B where\nimport A\nbar :: Char\nbar = foo"
      let withErrorAText = "module A where\nfoo :: String\nfoo = 42"
      ([_, r1, r3], _) <- runIde
        [ rebuildAll withCodegen
        , rebuildOne (fpB, withErrorBText)  noCodegen

        --, rebuildOne (fpA, withErrorAText)  noCodegen
        --, rebuildOne (fpA, moduleAText <> "\nzoo=1")  noCodegen
        , rebuildMany [(fpA, moduleAText <> "\nzoo=1")]  noCodegen
        ]

      -- No hint in the error for moduleName.
      r1 `expectErrorsFor` "B"
      r3 `expectErrorsFor` "B"

    -- xit (pfx "handles ModuleNotFound error that prevents make") $ do
    --   compileProject

    --   text <- Test.readModuleFailText mn
    --   fp <- moduleFailPath mn
    --   -- fp' <- modulePath mnRebuildSpecWithDeps

    --   let ideConfig = Test.defConfig
    --         { confGlobs = ["src/**/*.purs", "src/RebuildSpecFailedImport.fail"]
    --         }
    --   let fixModule = Text.replace "import Something as Something" ""

    --   ([r0, r1], _) <- Test.inProject $
    --     Test.runIde' ideConfig emptyIdeState
    --       [ rebuildAll jsCodegen
    --       -- , rebuildOne (fp, fixModule text)  noCodegen
    --       , rebuildMany [(fp, fixModule text)]  noCodegen
    --       --, rebuildMany []  noCodegen
    --       ]

    --   expectCompiledCount r0 (modulesTotal + 1)
    --   expectCompiledCount r1 1


  describe "Rebuilding single modules" $ before cleanUp $ do

    it "rebuilds a correct module without dependencies successfully" $ do
      compileProject

      fpA <- moduleAbsPath "A"
      ([_, r], _) <- runIde
        [ load ["A"]
        , rebuild fpA
        ]

      expectNoErrors r

    it "fails to rebuild an incorrect module without dependencies and returns the errors" $ do
      writeModule "A" "module A where x = 1 + 1"
      compileProject

      fpA <- moduleAbsPath "A"
      ([_, r], _) <- runIde
        [ load ["A"]
        , rebuild fpA
        ]

      expectErrors r


  -- H.xdescribe "Rebuilding single modules" $ do

  --   it "rebuilds a correct module without dependencies successfully" $ do
  --     ([_, result], _) <- Test.inProject $
  --       Test.runIde [ load ["RebuildSpecSingleModule"]
  --                   , rebuild "RebuildSpecSingleModule.purs"
  --                   ]

  --     result `shouldSatisfy` isRight

  --   it "fails to rebuild an incorrect module without dependencies and returns the errors" $ do
  --     ([result], _) <- Test.inProject $
  --       Test.runIde [ rebuild "RebuildSpecSingleModule.fail" ]

  --     result `shouldSatisfy` isLeft

  --   it "rebuilds a correct module with its dependencies successfully" $ do
  --     ([_, result], _) <- Test.inProject $
  --       Test.runIde [ load ["RebuildSpecWithDeps", "RebuildSpecDep"]
  --                   , rebuild "RebuildSpecWithDeps.purs"
  --                   ]

  --     result `shouldSatisfy` isRight

  --   it "rebuilds a correct module that has reverse dependencies" $ do
  --     ([_, result], _) <- Test.inProject $
  --       Test.runIde [ load ["RebuildSpecWithDeps"], rebuild "RebuildSpecDep.purs" ]

  --     result `shouldSatisfy` isRight

  --   it "fails to rebuild a module if its dependencies are not loaded" $ do
  --     ([_, result], _) <- Test.inProject $
  --       Test.runIde [ load ["RebuildSpecWithDeps"], rebuild "RebuildSpecWithDeps.purs" ]

  --     result `shouldSatisfy` isLeft

  --   it "rebuilds a correct module with a foreign file" $ do
  --     ([_, result], _) <- Test.inProject $
  --       Test.runIde [ load ["RebuildSpecWithForeign"], rebuild "RebuildSpecWithForeign.purs" ]

  --     result `shouldSatisfy` isRight

  --   it "fails to rebuild a module with a foreign import but no file" $ do
  --     ([result], _) <- Test.inProject $
  --       Test.runIde [ rebuild "RebuildSpecWithMissingForeign.fail" ]

  --     result `shouldSatisfy` isLeft

  --   it "completes a hidden identifier after rebuilding" $ do
  --     ([_, Right (CompletionResult [ result ])], _) <- Test.inProject $
  --       Test.runIde [ rebuildSync "RebuildSpecWithHiddenIdent.purs"
  --                   , Complete [] (flexMatcher "hid") (Just (Test.mn "RebuildSpecWithHiddenIdent")) defaultCompletionOptions]

  --     complIdentifier result `shouldBe` "hidden"

  --   it "uses the specified `actualFile` for location information" $ do
  --     ([_, Right (CompletionResult [ result ])], _) <- Test.inProject $
  --       Test.runIde'
  --         Test.defConfig
  --         emptyIdeState
  --         [ RebuildSync ("src" </> "RebuildSpecWithHiddenIdent.purs") (Just "actualFile") defaultTarget
  --         , Complete [] (flexMatcher "hid") (Just (Test.mn "RebuildSpecWithHiddenIdent")) defaultCompletionOptions]

  --     map spanName (complLocation result) `shouldBe` Just "actualFile"

  --   it "doesn't produce JS when an empty target list is supplied" $ do
  --     exists <- Test.inProject $ do
  --       let indexJs = "output" </> "RebuildSpecSingleModule" </> "index.js"
  --       removePathForcibly ("output" </> "RebuildSpecSingleModule")

  --       _ <- Test.runIde [ RebuildSync ("src" </> "RebuildSpecSingleModule.purs") Nothing Set.empty ]
  --       doesFileExist indexJs

  --     exists `shouldBe` False

  --   it "does produce corefn if it's a codegen target" $ do
  --     exists <- Test.inProject $ do
  --       let corefn = "output" </> "RebuildSpecSingleModule" </> "corefn.json"
  --       removePathForcibly ("output" </> "RebuildSpecSingleModule")

  --       _ <- Test.runIde [ RebuildSync ("src" </> "RebuildSpecSingleModule.purs") Nothing (Set.singleton P.CoreFn) ]
  --       doesFileExist corefn

  --     exists `shouldBe` True
  where
    pfx = (<>) "rebuild2: "

    moduleAText = "module A where\nfoo :: Int\nfoo = 42"
    -- B depends on A
    moduleBText = "module B where\nimport A\nbar :: Int\nbar = foo"
    -- C is independent
    moduleCText = "module C where\nzero :: Int\nzero = 0"

    mnRebuildSpecDep = Test.mn "RebuildSpecDep"
    mnRebuildSpecWithDeps = Test.mn "RebuildSpecWithDeps"

    withCodegen = Set.singleton P.JS
    noCodegen = Set.empty

    -- Updates source code of RebuildSpecDep module.
    changeDepValue =
      Text.replace "42" "43"

    changeDepType =
      Text.replace "42" "\"42\"" . Text.replace "Int" "String"

    --

    cleanUp = do
      Test.deleteOutputFolder
      Test.deleteSrcFolder
      writeDefaultModules

    compileProject = void $ Test.compileTestProject' "_src"

    ideConfig = Test.defConfig
      { confGlobs = ["_src/**/*.purs"]
      }

    writeModule = Test.writeModule . Test.mn
    writeForeignModule = Test.writeForeignModule . Test.mn

    writeDefaultModules = do
      writeModule "A" moduleAText
      writeModule "B" moduleBText
      writeModule "C" moduleCText

    runIde = Test.inProject . Test.runIde' ideConfig emptyIdeState
    runIdeWithState st = Test.inProject . Test.runIde' ideConfig st

    moduleAbsPath = Test.moduleAbsolutePath . Test.mn

    --moduleAbsPaths = traverse moduleAbsPath
    modulePath = Test.moduleAbsolutePath
    moduleFailPath = Test.moduleFailAbsolutePath
    moduleOutputPath mn = Text.unpack ("output/" <> mn <> "/index.js")

    checkFileExists relPath = do
      exists <- Test.inProject $ doesFileExist relPath
      exists `shouldBe` True

    checkFileDoesNotExist relPath = do
      exists <- Test.inProject $ doesFileExist relPath
      exists `shouldBe` False

    getModuleOutputTimestamp mn = do
      let relPath = moduleOutputPath mn
      Test.inProject $ getModificationTime relPath

    readModuleOutputText mn = do
      let relPath = moduleOutputPath mn
      Test.inProject $ readUTF8FileT relPath

    expectNoTimestampChangesAfter :: FilePath -> IO a -> IO a
    expectNoTimestampChangesAfter relPath action = do
      t1 <- Test.inProject $ getModificationTime relPath
      res <- action
      t2 <- Test.inProject $ getModificationTime relPath
      t1 `shouldBe` t2
      pure res
    --

    wrongResult r = error ("Wrong command result type: " <> show r)

    getCompletions r =
        case r of
          Right (CompletionResult c) -> c
          _ -> wrongResult r

    returnsCompletionIdents r idents =
        complIdentifier <$> getCompletions r `shouldBe` idents

    getCompiled r =
      case r of
        Right (Rebuild2Result compiled (_, _)) -> compiled
        Left (Rebuild2Error compiled (_, _)) -> compiled
        _ -> wrongResult r

    getErrors r =
      case r of
        Right (RebuildSuccess _) -> mempty
        Left (RebuildError _ errs) -> errs

        Right (Rebuild2Result _ (_, errors)) -> errors
        Left (Rebuild2Error _ (_, errors)) -> errors
        _ -> wrongResult r

    getWarnings r =
      case r of
        Right (RebuildSuccess warns) -> warns
        Left (RebuildError _ _) -> mempty

        Right (Rebuild2Result _ (warns, _)) -> warns
        Left (Rebuild2Error _ (warns, _)) -> warns
        _ -> wrongResult r

    expectErrors r =
      getErrors r `shouldSatisfy` Errors.nonEmpty

    expectErrorsForFp r fp =
      fp `shouldSatisfy` flip Set.member
        (Set.fromList $ P.spanName <$> mapMaybe P.suggestionSpan (P.runMultipleErrors (getErrors r)))

    expectErrorsFor' fn  r mn =
      Test.mn mn `fn` flip Set.member
        (Set.fromList $ mapMaybe P.errorModule (P.runMultipleErrors (getErrors r)))

    expectErrorsFor = expectErrorsFor' shouldSatisfy
    expectNoErrorsFor = expectErrorsFor' shouldNotSatisfy

    ---expectErrorsNoFor

    expectWarningsFor r mn =
      Test.mn mn `shouldSatisfy` flip Set.member
        (Set.fromList $ mapMaybe P.errorModule (P.runMultipleErrors (getWarnings r)))

    expectNoErrors r =
      getErrors r `shouldSatisfy` not . Errors.nonEmpty

    expectNoWarnings r = do
      getWarnings r `shouldSatisfy` not . Errors.nonEmpty

    expectCompiledCount r count =
      length (getCompiled r) `shouldBe` count

    expectCompiledAll r = do
      files <- Test.listModuleFiles
      expectCompiledCount r (length files)

    expectCompiled r paths = do
      sort (getCompiled r) `shouldBe` sort paths


