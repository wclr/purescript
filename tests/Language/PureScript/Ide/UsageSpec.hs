module Language.PureScript.Ide.UsageSpec where

import           Protolude

import           Control.Exception (throw)
import           Data.List ((!!), findIndices)
import qualified Data.Text as Text
import qualified Data.Text.Internal.Search as Search
import           Language.PureScript.Ide.Command
import           Language.PureScript.Ide.Types
import qualified Language.PureScript.Ide.Test as Test
import qualified Language.PureScript as P
import           System.FilePath
import           System.IO.UTF8 (readUTF8FileT)
import           Test.Hspec

load :: [P.ModuleName] -> Command
load = LoadSync

usage :: P.ModuleName -> Text -> IdeNamespace -> Command
usage = FindUsages

-- | Compares if usages with expected result
shouldBeUsages :: [P.SourceSpan] -> [(FilePath, TextSpan)] -> Expectation
shouldBeUsages usages expected  =
  let
    normPath = map (\c -> if c == '\\' then '/' else c)
    toRangePos pos = show (P.sourcePosLine pos) ++ ":" ++ show (P.sourcePosColumn pos)
    toRange span =  toRangePos (P.spanStart span) ++ "-" ++ toRangePos (P.spanEnd span)
    toFp projectDir span = makeRelative (projectDir </> "src") (P.spanName span)
    toExpected projectDir span =
      (normPath $ toFp projectDir span, Text.pack $ toRange span)
  in
    do
      projectDir <- Test.getProjectDirectory
      toExpected projectDir <$> usages `shouldBe` expected

-- | Loads passed modules and gets usages
getUsages :: [P.ModuleName] -> (P.ModuleName, Text, IdeNamespace) -> IO [P.SourceSpan]
getUsages modules (module', ident, ns) = do
    ([_, Right (UsagesResult usages)], _) <- Test.inProject $
      Test.runIde [ load modules
                  , usage module' ident ns
                  ]
    pure usages

-- | Helper to make test assertion. Maybe need to remove this.
checkUsages ::
  [P.ModuleName] -> (P.ModuleName, Text, IdeNamespace) -> [(FilePath, TextSpan)] -> Expectation
checkUsages modules ident expected = do
  usages <- getUsages modules ident
  usages `shouldBeUsages` expected

findTextSpan :: Text -> (Text, Text) -> Maybe Span
findTextSpan moduleText (linePtn, identPtn) = do
  -- lineIdx <- findIndex (Text.isInfixOf linePtn) textLines

  -- find lines containing linePtn
  let lineIdxs = findIndices (Text.isInfixOf linePtn) textLines
  -- find the line containing identPattern
  lineIdx <- find (Text.isInfixOf identPtn . (textLines !!)) lineIdxs

  let lineText = textLines !! lineIdx

  -- first try to search the ident pattern inside the line pattern
  idx <- case head $ Search.indices identPtn linePtn of
    Just insideIdx
      | Just linePtnIdx <- head $ Search.indices linePtn lineText ->
        Just (linePtnIdx + insideIdx)
    _ ->
      -- search inside the whole line text
      head $ Search.indices identPtn lineText
  let li = lineIdx + 1
  let (si, ei) = (idx + 1, idx + 1 + Text.length identPtn)
  pure ((li, si), (li, ei))

 where
  textLines = Text.lines moduleText

moduleNameRelPath ::  P.ModuleName -> FilePath
moduleNameRelPath (P.ModuleName m) =
  Text.unpack (Text.replace "." "/" m <> ".purs")

moduleNameToSrcPath :: FilePath -> P.ModuleName -> FilePath
moduleNameToSrcPath dir m =
  dir </> "src" </>  moduleNameRelPath m

data SpanFindException = SpanFindException Text deriving (Show)

instance Exception SpanFindException

-- maybe later to remove it if multi-line won't be the case
data SearchPattern
  = SingleLine (Text, Text)
  | MultiLine (Text, Text) (Text, Text)
  deriving (Show)

-- start/end line/column
type Span = ((Int, Int), (Int, Int))

type TextSpan = Text

renderSpan :: Span -> TextSpan
renderSpan ((sl, sc), (el, ec)) =
  show sl <> ":" <> show sc <> "-" <> show el <> ":" <> show ec

-- | Builds a text span finder in purs module sources.
--
-- Finder function takes:
-- - purs module name
-- - tuple of pattern for a string which contains target and a pattern of target
-- identifier/text
--
-- This helps to avoid manual setting/adjusting of expected span values, which
-- is needed in case of structural changes in tested sources.
makeSpanFinder ::
  [P.ModuleName] -> IO (P.ModuleName -> SearchPattern -> (FilePath, TextSpan))
makeSpanFinder modules = do
  projectDir <- Test.getProjectDirectory
  mods <- mapM (loadText projectDir) modules
  pure $ ret mods
 where
  ret mods m patterns =
    ( moduleNameRelPath m
    , renderSpan $
        fromMaybe (throw $ SpanFindException errMsg) $
          getSpan $
            fromMaybe (throw $ SpanFindException notLoadedMsg) mbMod
    )
   where
    toName (P.ModuleName n) = n
    notLoadedMsg = "Source code of " <> toName m <> " is not loaded"
    errMsg = "Could not find text patterns " <> show patterns <> " in " <> toName m
    getSpan (_, txt) =
      case patterns of
        SingleLine ptn ->
          findTextSpan txt ptn
        MultiLine startPtn endPtn -> do
          (s, _) <- findTextSpan txt startPtn
          (_, e) <- findTextSpan txt endPtn
          pure (s, e)
    mbMod = find ((== m) . fst) mods

  loadText dir m = do
    txt <- readUTF8FileT (moduleNameToSrcPath dir m)
    pure (m, txt)

-- | Tests on purs sources in tests/support/pscide/src/FindUsage
spec :: Spec
spec = beforeAll (makeSpanFinder allModules) $
  describe "Finding Usages" $ do
    it "finds a usage of exported function (usageId)" $ \span -> do
      checkUsages
        [mFindUsage, mDefinition, mReexport]
        (mDefinition, "usageId", IdeNSValue)
        --
        [ span mFindUsage $ s ("import FindUsage.Definition", "usageId")
        , span mFindUsage $ s ("usageFn = usageId", "usageId")
        , span mDefinition $ s ("toBeReexported = usageId", "usageId")
        ]

    it "finds a usage of exported function (usageId2)" $ \span -> do
      usages <- getUsages
        [mFindUsage, mDefinition, mReexport]
        (mDefinition, "usageId2", IdeNSValue)
      usages `shouldBeUsages`
        [ span mFindUsage $ s ("import FindUsage.Definition", "usageId2")
        , span mFindUsage $ s ("usageId2 =", "D.usageId2")
        ]

    it "finds a usage of simple recursive" $ \span -> do
      usages <- getUsages
        [mRecursive]
        (mRecursive, "recursiveUsage", IdeNSValue)
      usages `shouldBeUsages`
        [span mRecursive $ s ("-> recursiveUsage x", "recursiveUsage")]

    it "ignores a usage of locally shadowed recursive" $ \_ -> do
      usages <- getUsages
        [mRecursiveShadowed]
        (mRecursiveShadowed, "recursiveUsage", IdeNSValue)
      usages `shouldBe` []

    it "finds a usage of data type (Usage)" $ \span -> do
      usages <- getUsages
        [mFindUsage, mDefinition, mReexport]
        (mDefinition, "Usage", IdeNSType)
      usages `shouldBeUsages`
        [ span mFindUsage $ s ("import FindUsage.Definition", "Usage(Used)")
        , span mFindUsage $ s ("import FindUsage.Definition (Usage", "Usage(..)")
        , span mFindUsage $ s ("usagePatternMatch :: Usage", "Usage")
        , span mFindUsage $ s ("-> D.Usage", "D.Usage")
        , span mFindUsage $ s ("-> (x :: D.Usage)", "D.Usage")
        , span mFindUsage $ s ("Find = Find Usage", "Usage")
        , span mDefinition $ s ("= Array Usage", "Usage")
        -- for infix case it finds the whole line
        , span mDefinition $ s ("infixl 2 type Usage as <$%>", "infixl 2 type Usage as <$%>")
        , span mDefinition $ s ("use :: Usage", "Usage")
        ]

    it "finds a usage of data constructor (Used)" $ \span -> do
      usages <- getUsages
        [mFindUsage, mDefinition, mReexport]
        (mDefinition, "Used", IdeNSValue)
      usages `shouldBeUsages`
        -- for data cons while pattern position is returned
        [ span mFindUsage $ s ("import FindUsage.Definition", "Usage(Used)")
        , span mFindUsage $ s ("import FindUsage.Definition (Usage", "Usage(..)")
        , span mFindUsage $ s ("D.Used _ ->", "D.Used _")
        , span mFindUsage $ s ("= Used 0", "Used")
        ]

    it "finds a usage of data constructor (Usage)" $ \span -> do
      usages <- getUsages
        [mFindUsage, mDefinition, mReexport]
        (mDefinition, "Usage", IdeNSValue)
      usages `shouldBeUsages`
        -- even if data constructor name is not in import it still will be returned
        [ span mFindUsage $ s ("import FindUsage.Definition (Usage", "Usage(..)")
        , span mDefinition $ s ("infixl 2 Usage as $%", "infixl 2 Usage as $%")
        ]

    it "finds a usage of constructor alias operator ($%)" $ \span -> do
      usages <- getUsages
        [mFindUsage, mDefinition, mReexport]
        (mDefinition, "$%", IdeNSValue)
      usages `shouldBeUsages`
        [ span mFindUsage $ s ("import FindUsage.Definition", "($%)")
        , span mFindUsage $ s ("_ $% _ ->", "$%")
        ]

    it "finds a usage of reexported function" $ \span -> do
      usages <- getUsages
        [mFindUsage, mDefinition, mReexport, mReexport2]
        (mDefinition, "toBeReexported", IdeNSValue)
      usages `shouldBeUsages`
        [ span mFindUsage $ s ("import FindUsage.Reexport2", "toBeReexported")
        , span mFindUsage $ s ("usageId toBeReexported", "toBeReexported")
        -- usage in reexport module it self
        , span mReexport $ s ("import FindUsage.Definition", "toBeReexported")
        , span mReexport $ s ("= X.toBeReexported", "X.toBeReexported")
        , span mReexport2 $ s ("= X.toBeReexported", "X.toBeReexported")
        ]

    it "finds a usage of newtype constructor" $ \span -> do
      usages <- getUsages
        [mFindUsage, mDefinition]
        (mFindUsage, "Find", IdeNSValue)
      usages `shouldBeUsages`
        [span mFindUsage $ s ("use = Find", "Find")]

    it "finds a usage of newtype type" $ \span -> do
      usages <- getUsages
        [mFindUsage, mDefinition]
        (mFindUsage, "Find", IdeNSType)
      usages `shouldBeUsages`
        [span mFindUsage $ s ("instance UsageTC Find", "Find")]

    it "finds a usage of type class (instance)" $ \span -> do
      usages <- getUsages
        [mFindUsage, mDefinition]
        (mDefinition, "UsageTC", IdeNSType)
      usages `shouldBeUsages`
        -- for type class instance returns whole instance span
        [ span mFindUsage $ s ("import FindUsage.Definition", "class UsageTC")
        , span mFindUsage $ m ("instance UsageTC", "instance") ("use = Find", "Find")
        ]

    -- it "finds a usage of type class method (use)" $ \span -> do
    --   usages <- getUsages
    --     [mFindUsage, mDefinition]
    --     (mDefinition, "use", IdeNSValue)
    --   usages `shouldBeUsages`
    --     [ span mFindUsage $ s ("use = Find", "use") ]

    where
    mFindUsage = Test.mn "FindUsage"
    mDefinition = Test.mn "FindUsage.Definition"
    mReexport = Test.mn "FindUsage.Reexport"
    mReexport2 = Test.mn "FindUsage.Reexport2"
    mRecursive = Test.mn "FindUsage.Recursive"
    mRecursiveShadowed = Test.mn "FindUsage.RecursiveShadowed"

    allModules =
      [ mFindUsage
      , mDefinition
      , mReexport
      , mReexport2
      , mRecursive
      , mRecursiveShadowed
     ]
    s = SingleLine
    m = MultiLine
