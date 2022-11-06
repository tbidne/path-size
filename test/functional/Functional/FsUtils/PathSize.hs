-- | Tests for logging.
--
-- @since 0.1
module Functional.FsUtils.PathSize
  ( tests,
  )
where

import Data.ByteString.Lazy qualified as BSL
import Data.Text.Encoding qualified as TEnc
import FsUtils.PathSize qualified as PathSize
import System.Directory qualified as Dir
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "FsUtils.PathSize"
    [ calculatesSizes
    ]

calculatesSizes :: TestTree
calculatesSizes = goldenVsStringDiff desc diff gpath $ do
  testDir <- (</> "test/functional/data") <$> Dir.getCurrentDirectory
  result <- PathSize.display <$> PathSize.findLargestPaths mempty testDir
  pure $ BSL.fromStrict $ TEnc.encodeUtf8 result
  where
    desc = "Calculates sizes correctly"
    gpath = goldenPath </> "sizes.golden"

goldenPath :: FilePath
goldenPath = "test/functional/Functional/FsUtils/"

diff :: FilePath -> FilePath -> [FilePath]
diff ref new = ["diff", "-u", ref, new]
