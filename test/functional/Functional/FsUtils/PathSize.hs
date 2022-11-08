-- | Tests for logging.
--
-- @since 0.1
module Functional.FsUtils.PathSize
  ( tests,
  )
where

import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLEnc
import FsUtils.Data.PathSizeConfig (PathSizeConfig (searchAll))
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
    [ calculatesSizes,
      calculatesAll
    ]

calculatesSizes :: TestTree
calculatesSizes = goldenVsStringDiff desc diff gpath $ do
  testDir <- (</> "test/functional/data") <$> Dir.getCurrentDirectory
  result <- PathSize.display <$> PathSize.findLargestPaths mempty testDir
  currDir <- Dir.getCurrentDirectory
  pure $ replaceDir currDir result
  where
    desc = "Calculates sizes correctly"
    gpath = goldenPath </> "sizes.golden"

calculatesAll :: TestTree
calculatesAll = goldenVsStringDiff desc diff gpath $ do
  testDir <- (</> "test/functional/data") <$> Dir.getCurrentDirectory
  result <- PathSize.display <$> PathSize.findLargestPaths cfg testDir
  currDir <- Dir.getCurrentDirectory
  pure $ replaceDir currDir result
  where
    cfg = mempty {searchAll = True}
    desc = "Includes hidden files"
    gpath = goldenPath </> "all.golden"

goldenPath :: FilePath
goldenPath = "test/functional/Functional/FsUtils/"

diff :: FilePath -> FilePath -> [FilePath]
diff ref new = ["diff", "-u", ref, new]

-- HACK: Our naive golden tests require exact string quality, which is a
-- problem since the full paths are non-deterministic, depending on the
-- environment.
replaceDir :: FilePath -> Text -> BSL.ByteString
replaceDir fp =
  TLEnc.encodeUtf8
    . TL.replace (TL.pack fp) "<dir>"
    . TL.fromStrict
