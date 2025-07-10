{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Functional.PathSize
  ( tests,
  )
where

import Control.Exception (Exception (displayException))
import Control.Exception.Utils (trySync)
import Control.Monad.Catch (MonadCatch, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (Foldable (toList))
import Data.HashSet qualified as HSet
import Data.List qualified as L
import Data.Sequence.NonEmpty (NESeq)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)
import Effects.Concurrent.Async (MonadAsync)
import Effects.FileSystem.PathReader (MonadPathReader)
import Effects.FileSystem.PathReader qualified as RDir
#if POSIX
import Effects.System.Posix (MonadPosix (getSymbolicLinkStatus))
import System.OsString.Internal.Types (OsString (OsString))
#else
import Effects.System.PosixCompat (MonadPosixCompat (getSymbolicLinkStatus))
#endif
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Effects.FileSystem.FileWriter (ByteString)
import FileSystem.IO qualified as FS.IO
import FileSystem.OsPath (OsPath, osp, ospPathSep, (</>))
import FileSystem.OsPath qualified as FS.OsPath
import FileSystem.UTF8 qualified as FS.UTF8
import GoldenParams
  ( GoldenParams
      ( MkGoldenParams,
        mConfig,
        mDisplayConfig,
        runner,
        testDesc,
        testName,
        testPath
      ),
  )
import PathSize
  ( DisplayConfig (color, format),
    DisplayFormat (DisplayFormatSingle),
    PathE,
    PathSizeResult (PathSizeFailure, PathSizePartial, PathSizeSuccess),
    Strategy (Sync),
    defaultDisplayConfig,
  )
import PathSize qualified
import PathSize.Data.Config
  ( Config
      ( MkConfig,
        exclude,
        filesOnly,
        ignoreDirIntrinsicSize,
        maxDepth,
        numPaths,
        searchAll,
        stableSort,
        strategy
      ),
  )
import PathSize.Data.SubPathData.Internal
  ( DisplayFormat (DisplayFormatTabular),
    SubPathData,
  )
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff)

tests :: TestTree
tests =
  testGroup
    "FsSize.PathSize"
    [ calculatesSizes,
      testDisplaysTabular,
      calculatesAll,
      calculatesExcluded,
      calculatesFilesOnly,
      calculatesIgnoreDirIntrinsicSize,
      calculatesDepthN 0,
      calculatesDepthN 1,
      calculatesDepthN 2,
      exceptionTests
    ]

calculatesSizes :: TestTree
calculatesSizes = testGoldenParams params
  where
    params =
      MkGoldenParams
        { mConfig = Nothing,
          mDisplayConfig = Nothing,
          testDesc = "Calculates sizes correctly",
          testName = [osp|calculatesSizes|],
          testPath = successTestDir,
          runner = runTest
        }

testDisplaysTabular :: TestTree
testDisplaysTabular = testGoldenParams params
  where
    params =
      MkGoldenParams
        { mConfig = Nothing,
          mDisplayConfig = Just $ displayConfig {format = DisplayFormatTabular},
          testDesc = "Displays tabular format",
          testName = [osp|testDisplaysTabular|],
          testPath = successTestDir,
          runner = runTest
        }

calculatesAll :: TestTree
calculatesAll = testGoldenParams params
  where
    params =
      MkGoldenParams
        { mConfig = Just cfg,
          mDisplayConfig = Nothing,
          testDesc = "Includes hidden files",
          testName = [osp|calculatesAll|],
          testPath = successTestDir,
          runner = runTest
        }

    cfg = baseConfig {searchAll = True}

calculatesExcluded :: TestTree
calculatesExcluded = testGoldenParams params
  where
    params =
      MkGoldenParams
        { mConfig = Just cfg,
          mDisplayConfig = Nothing,
          testDesc = "Excludes paths",
          testName = [osp|calculatesExcluded|],
          testPath = successTestDir,
          runner = runTest
        }

    cfg = baseConfig {exclude = HSet.fromList [[osp|d2|], [osp|f2|]]}

calculatesFilesOnly :: TestTree
calculatesFilesOnly = testGoldenParams params
  where
    params =
      MkGoldenParams
        { mConfig = Just cfg,
          mDisplayConfig = Nothing,
          testDesc = "Includes only files",
          testName = [osp|calculatesFilesOnly|],
          testPath = successTestDir,
          runner = runTest
        }
    cfg = baseConfig {filesOnly = True}

calculatesIgnoreDirIntrinsicSize :: TestTree
calculatesIgnoreDirIntrinsicSize = testGoldenParams params
  where
    params =
      MkGoldenParams
        { mConfig = Just cfg,
          mDisplayConfig = Nothing,
          testDesc = "Ignores dir intrinsic size",
          testName = [osp|calculatesIgnoreDirIntrinsicSize|],
          testPath = successTestDir,
          runner = runTest
        }

    cfg = baseConfig {ignoreDirIntrinsicSize = True}

calculatesDepthN :: Word16 -> TestTree
calculatesDepthN n = testGoldenParams params
  where
    params =
      MkGoldenParams
        { mConfig = Just cfg,
          mDisplayConfig = Nothing,
          testDesc = "Calculates depth = " <> show n,
          testName = [osp|calculatesDepth_|] <> FS.OsPath.unsafeEncode (show n),
          testPath = successTestDir,
          runner = runTest
        }

    cfg = baseConfig {maxDepth = Just n}

exceptionTests :: TestTree
exceptionTests =
  testGroup
    "Exceptions"
    [ testsPartial,
      testsFailure
    ]

testsPartial :: TestTree
testsPartial = testGoldenParams params
  where
    params =
      MkGoldenParams
        { mConfig = Nothing,
          mDisplayConfig = Nothing,
          testDesc = "Partial success",
          testName = [osp|testsPartial|],
          testPath = partialTestDir,
          runner = runTest
        }

testsFailure :: TestTree
testsFailure = testGoldenParams params
  where
    params =
      MkGoldenParams
        { mConfig = Nothing,
          mDisplayConfig = Nothing,
          testDesc = "Failure",
          testName = [osp|testsFailure|],
          testPath = failureTestDir,
          runner = runTest
        }

newtype FuncIO a = MkFuncIO (IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadAsync,
      MonadCatch,
      MonadIO,
      MonadThrow
    )
    via IO

#if POSIX

instance MonadPosix FuncIO where
  getSymbolicLinkStatus path = case pathToErr (OsString path) of
    Just err -> throwM err
    Nothing -> liftIO $ getSymbolicLinkStatus path

#else

instance MonadPosixCompat FuncIO where
  getSymbolicLinkStatus path = case pathToErr (FS.OsPath.unsafeEncode path) of
    Just err -> throwM err
    Nothing -> liftIO $ getSymbolicLinkStatus path

#endif

pathToErr :: OsPath -> Maybe E
pathToErr path
  | path == basePath </> [ospPathSep|partial/d1/is-dir-err|] = Just $ MkE "dir err"
  | path == basePath </> [ospPathSep|partial/size-err|] = Just $ MkE "bad size"
  | path == basePath </> [ospPathSep|failure|] = Just $ MkE "does not exist"
  | path == basePath </> [ospPathSep|partial/d1/is-sym-link-err|] = Just $ MkE "sym link err"
  | otherwise = Nothing
  where
    basePath = [ospPathSep|test/functional/data|]

newtype E = MkE String
  deriving stock (Show)

instance Exception E where
  displayException (MkE s) = s

instance MonadPathReader FuncIO where
  listDirectory = liftIO . RDir.listDirectory

runFuncIO :: FuncIO a -> IO a
runFuncIO (MkFuncIO io) = io

testGoldenParams :: GoldenParams -> TestTree
testGoldenParams goldenParams =
  goldenDiffCustom goldenParams.testDesc goldenPath actualPath $ do
    let config = fromMaybe baseConfig goldenParams.mConfig
        dispConfig = fromMaybe displayConfig goldenParams.mDisplayConfig

    result <- goldenParams.runner config dispConfig goldenParams.testPath
    writeActualFile result
  where
    outputPathStart =
      FS.OsPath.unsafeDecode $
        [ospPathSep|test/functional/goldens|] </> goldenParams.testName

    actualPath = outputPathStart <> ext <> ".actual"
    goldenPath = outputPathStart <> ext <> ".golden"

    toBS :: Text -> ByteString
    toBS = (<> "\n") . FS.UTF8.encodeUtf8

    writeActualFile :: Text -> IO ()
    writeActualFile result =
      FS.IO.writeBinaryFileIO (FS.OsPath.unsafeEncode actualPath) (toBS result)

runTest :: Config -> DisplayConfig -> OsPath -> IO Text
runTest cfg dispCfg testDir = do
  trySync (runTestNoCatch cfg testDir) <&> \case
    Right (PathSizeSuccess spd) -> display spd
    Right (PathSizePartial errs spd) -> fmtErrs errs <> "\n" <> display spd
    Right (PathSizeFailure errs) -> fmtErrs errs
    Left ex -> T.pack $ displayException ex
  where
    display = PathSize.display dispCfg

    fmtErrs :: NESeq PathE -> Text
    fmtErrs =
      T.unlines
        . fmap (T.pack . displayException)
        -- sorted because error order is non-deterministic (consequence of
        -- subpath non-determinism)
        . L.sort
        . toList

runTestNoCatch :: Config -> OsPath -> IO (PathSizeResult SubPathData)
runTestNoCatch cfg = runFuncIO . PathSize.findLargestPaths cfg

successTestDir :: OsPath
successTestDir = [ospPathSep|test/functional/data/success|]

partialTestDir :: OsPath
partialTestDir = [ospPathSep|test/functional/data/partial|]

failureTestDir :: OsPath
failureTestDir = [ospPathSep|test/functional/data/failure|]

baseConfig :: Config
baseConfig =
  MkConfig
    { searchAll = False,
      maxDepth = Nothing,
      exclude = HSet.empty,
      filesOnly = False,
      ignoreDirIntrinsicSize = False,
      numPaths = Nothing,
      -- True because we want tests to be deterministic
      stableSort = True,
      strategy = Sync
    }

displayConfig :: DisplayConfig
displayConfig =
  defaultDisplayConfig
    { color = False,
      format = DisplayFormatSingle
    }

{- ORMOLU_DISABLE -}

ext :: FilePath
ext =
#if WINDOWS
  "_windows"
#elif OSX
  "_osx"
#else
  "_linux"
#endif

{- ORMOLU_ENABLE -}

-- | We use a custom diff as this will print the actual diff to stdout,
-- whereas ordinary goldenVsFile will merely print something like
-- 'files are different'. The former is especially useful when we do not have
-- easy access to the diff files e.g. CI.
goldenDiffCustom :: TestName -> FilePath -> FilePath -> IO () -> TestTree
goldenDiffCustom x = goldenVsFileDiff x diffArgs
  where
    -- Apparently, the 'diff' program exists for windows and unix on CI. Thus
    -- the arguments ["diff", "-u" "--color=always", ref, new] also seem fine.
    -- Nevertheless, we use git as it is possibly more portable.
    diffArgs ref new =
      [ "git",
        "diff",
        "--exit-code",
        "--color=always",
        "--no-index",
        ref,
        new
      ]
