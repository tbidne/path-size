{-# LANGUAGE OverloadedLists #-}

-- | Tests for logging.
--
-- @since 0.1
module Functional.PathSize
  ( tests,
  )
where

import Control.Exception (Exception (displayException), throwIO)
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (Foldable (foldl'))
import Data.HashSet qualified as HSet
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLEnc
import GHC.Num.Natural (Natural)
import PathSize (PathSizeResult (..), SubPathData)
import PathSize qualified
import PathSize.Data.Config (Config (..))
import PathSize.Exception (PathE)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "FsSize.PathSize"
    [ calculatesSizes,
      calculatesReverse,
      calculatesAll,
      calculatesExcluded,
      calculatesFilesOnly,
      calculatesDepthN 0,
      calculatesDepthN 1,
      calculatesDepthN 2
    ]

calculatesSizes :: TestTree
calculatesSizes = goldenVsStringDiff desc diff gpath $ do
  testDir <- (</> "test/functional/data") <$> Dir.getCurrentDirectory
  result <- PathSize.display False <$> runTest mempty testDir
  currDir <- Dir.getCurrentDirectory
  pure $ replaceDir currDir result
  where
    desc = "Calculates sizes correctly"
    gpath = goldenPath </> "sizes.golden"

calculatesReverse :: TestTree
calculatesReverse = goldenVsStringDiff desc diff gpath $ do
  testDir <- (</> "test/functional/data") <$> Dir.getCurrentDirectory
  result <- PathSize.display True <$> runTest mempty testDir
  currDir <- Dir.getCurrentDirectory
  pure $ replaceDir currDir result
  where
    desc = "Calculates reverse correctly"
    gpath = goldenPath </> "reverse.golden"

calculatesAll :: TestTree
calculatesAll = goldenVsStringDiff desc diff gpath $ do
  testDir <- (</> "test/functional/data") <$> Dir.getCurrentDirectory
  result <- PathSize.display False <$> runTest cfg testDir
  currDir <- Dir.getCurrentDirectory
  pure $ replaceDir currDir result
  where
    cfg = mempty {searchAll = True}
    desc = "Includes hidden files"
    gpath = goldenPath </> "all.golden"

calculatesExcluded :: TestTree
calculatesExcluded = goldenVsStringDiff desc diff gpath $ do
  testDir <- (</> "test/functional/data") <$> Dir.getCurrentDirectory
  result <- PathSize.display False <$> runTest cfg testDir
  currDir <- Dir.getCurrentDirectory
  pure $ replaceDir currDir result
  where
    cfg = mempty {exclude = HSet.fromList ["d2", "f2"]}
    desc = "Excludes paths"
    gpath = goldenPath </> "excluded.golden"

calculatesFilesOnly :: TestTree
calculatesFilesOnly = goldenVsStringDiff desc diff gpath $ do
  testDir <- (</> "test/functional/data") <$> Dir.getCurrentDirectory
  result <- PathSize.display False <$> runTest cfg testDir
  currDir <- Dir.getCurrentDirectory
  pure $ replaceDir currDir result
  where
    cfg = mempty {filesOnly = True}
    desc = "Includes only files"
    gpath = goldenPath </> "files-only.golden"

calculatesDepthN :: Natural -> TestTree
calculatesDepthN n = goldenVsStringDiff desc diff gpath $ do
  testDir <- (</> "test/functional/data") <$> Dir.getCurrentDirectory
  result <- PathSize.display False <$> runTest cfg testDir
  currDir <- Dir.getCurrentDirectory
  pure $ replaceDir currDir result
  where
    cfg = mempty {maxDepth = Just n}
    desc = "Calculates depth = " <> show n
    gpath = goldenPath </> "depth-" <> show n <> ".golden"

goldenPath :: FilePath
goldenPath = "test/functional/Functional/"

diff :: FilePath -> FilePath -> [FilePath]
diff ref new = ["diff", "-u", ref, new]

newtype StringE = MkStringE String
  deriving stock (Show)
  deriving (IsString) via String

instance Exception StringE where
  displayException (MkStringE e) = e

runTest :: Config -> FilePath -> IO SubPathData
runTest cfg testDir = do
  PathSize.findLargestPaths cfg testDir >>= \case
    PathSizeSuccess result -> pure result
    PathSizePartial errs _ -> throwIO $ MkStringE $ foldl' foldErrs "" errs
    PathSizeFailure errs -> throwIO $ MkStringE $ foldl' foldErrs "" errs
  where
    foldErrs :: String -> PathE -> String
    foldErrs s e = s <> displayException e

-- HACK: Our naive golden tests require exact string quality, which is a
-- problem since the full paths are non-deterministic, depending on the
-- environment.
replaceDir :: FilePath -> Text -> BSL.ByteString
replaceDir fp =
  TLEnc.encodeUtf8
    . TL.replace (TL.pack fp) "<dir>"
    . TL.fromStrict
