{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Provides common benchmarking functionality.
--
-- @since 0.1
module SafeRm.Bench.Common
  ( -- Benchmarks
    BenchmarkSuite (..),
    benchPathSizeRecursive,
    benchLargest10,
    benchDisplayPathSize,

    -- * Before/after
    setup,
    teardown,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad ((>=>))
import Data.Foldable (for_, traverse_)
import Data.HashSet qualified as HSet
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Data.Word (Word8)
import Effects.Exception (HasCallStack, addCS, throwCS)
import Effects.FileSystem.FileWriter (ByteString, writeBinaryFile)
import Effects.FileSystem.PathReader (getTemporaryDirectory)
import Effects.FileSystem.PathWriter
  ( createDirectoryIfMissing,
    removePathForcibly,
  )
import Effects.FileSystem.Utils (OsPath, osp, (</>), (</>!))
import Effects.FileSystem.Utils qualified as FsUtils
import Numeric.Data.Positive (mkPositive)
import PathSize
  ( Config
      ( MkConfig,
        exclude,
        filesOnly,
        maxDepth,
        numPaths,
        searchAll,
        stableSort,
        strategy
      ),
    PathSizeResult (PathSizeFailure, PathSizePartial, PathSizeSuccess),
    Strategy (Async, AsyncPool, Sync),
  )
import PathSize qualified
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')

{- HLINT ignore module "Redundant bracket" -}

-- | Holds benchmark functions for usage with criterion or tasty-bench.
--
-- @since 0.1
data BenchmarkSuite f b = MkBenchmarkSuite
  { bench :: String -> f -> b,
    bgroup :: String -> [b] -> b,
    nfIO :: forall a. (NFData a) => IO a -> f
  }

-- | Benchmark for general path searches over flat, spares, and dense
-- directory trees.
--
-- @since 0.1
benchPathSizeRecursive ::
  forall f b.
  -- | Benchmark functions to use.
  BenchmarkSuite f b ->
  -- | Benchmark strategies.
  NonEmpty Strategy ->
  -- | Test directory home.
  OsPath ->
  b
benchPathSizeRecursive MkBenchmarkSuite {..} strategies testDir =
  bgroup
    "findLargestPaths"
    [findLargest s (testDir </> [osp|dense-11|]) | s <- NE.toList strategies]
  where
    findLargest :: Strategy -> OsPath -> b
    findLargest strategy =
      bench desc'
        . nfIO
        . PathSize.findLargestPaths baseConfig {strategy}
      where
        desc' = strategyDesc strategy

-- | Benchmark for finding the largest N files in a dense directory tree.
--
-- @since 0.1
benchLargest10 ::
  BenchmarkSuite f b ->
  NonEmpty Strategy ->
  OsPath ->
  b
benchLargest10 MkBenchmarkSuite {..} strategies testDir =
  bgroup
    "takeLargest10"
    [ runLargestN
        s
        (mkPositive 10)
        (testDir </> [osp|dense-11|])
      | s <- NE.toList strategies
    ]
  where
    runLargestN strategy numPaths =
      bench desc'
        . nfIO
        . PathSize.findLargestPaths (baseConfig {numPaths, strategy})
      where
        desc' = strategyDesc strategy

-- | Benchmark for displaying results of a path search.
--
-- @since 0.1
benchDisplayPathSize ::
  BenchmarkSuite f b ->
  NonEmpty Strategy ->
  OsPath ->
  b
benchDisplayPathSize MkBenchmarkSuite {..} strategies testDir =
  bgroup
    "displayPathSize"
    [runDisplayPathSize s (testDir </> [osp|dense-11|]) | s <- NE.toList strategies]
  where
    runDisplayPathSize strategy =
      bench desc'
        . nfIO
        . (PathSize.findLargestPaths (baseConfig {strategy}) >=> displayResult)
      where
        desc' = strategyDesc strategy

    displayResult (PathSizeSuccess sbd) = pure $ PathSize.display False sbd
    displayResult (PathSizePartial (err :<|| _) _) = throwCS err
    displayResult (PathSizeFailure (err :<|| _)) = throwCS err

strategyDesc :: Strategy -> String
strategyDesc Sync = "Sync"
strategyDesc Async = "Async"
strategyDesc AsyncPool = "AsyncPool"

-- | Setups directories for benchmarking.
--
-- @since 0.1
setup :: (HasCallStack) => FilePath -> IO OsPath
setup base = do
  putStrLn "*** Starting setup ***"
  rootDir <- (</>! base) <$> getTemporaryDirectory
  createDirectoryIfMissing False rootDir

  createDenseDirs 11 (rootDir </> [osp|dense-11|]) files100

  putStrLn "*** Setup finished ***"
  pure rootDir
  where
    files100 = FsUtils.unsafeEncodeFpToOs . show @Int <$> [1 .. 100]

-- | Deletes directories created by 'setup'.
--
-- @since 0.1
teardown :: (HasCallStack) => OsPath -> IO ()
teardown rootDir =
  addCS $
    guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = removePathForcibly rootDir
    doNothing =
      putStrLn $ "*** Not cleaning up tmp dir: " <> show rootDir

-- | Creates a directory hierarchy of depth 2^n, where each directory contains
-- the parameter files.
createDenseDirs :: Word8 -> OsPath -> [OsPath] -> IO ()
createDenseDirs 0 root paths = createFlatDir root paths
createDenseDirs w root paths = do
  createFlatDir root paths
  traverse_ (\d -> createDenseDirs (w - 1) (root </> d) paths) subDirs
  where
    subDirs = [[osp|d1|], [osp|d2|]]

-- | Creates a single directory with the parameter files.
createFlatDir :: (HasCallStack) => OsPath -> [OsPath] -> IO ()
createFlatDir root paths = do
  createDirectoryIfMissing False root
  createFiles ((root </>) <$> paths)

-- | Creates empty files at the specified paths.
createFiles :: (HasCallStack) => [OsPath] -> IO ()
createFiles = createFileContents . fmap (,"")

-- | Creates files at the specified paths.
createFileContents ::
  (HasCallStack) =>
  [(OsPath, ByteString)] ->
  IO ()
createFileContents paths = for_ paths $
  \(p, c) -> addCS $ writeBinaryFile p c

baseConfig :: Config
baseConfig =
  MkConfig
    { searchAll = False,
      maxDepth = Nothing,
      exclude = HSet.empty,
      filesOnly = False,
      numPaths = Nothing,
      stableSort = False,
      strategy = Sync
    }
