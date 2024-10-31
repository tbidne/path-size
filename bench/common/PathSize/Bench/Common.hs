{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides common benchmarking functionality.
--
-- @since 0.1
module PathSize.Bench.Common
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
import Control.Monad.Catch (throwM)
import Data.Foldable (for_, traverse_)
import Data.HashSet qualified as HSet
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Data.Word (Word8)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.FileSystem.FileWriter.Dynamic qualified as FWD
import Effectful.FileSystem.FileWriter.Static
  ( ByteString,
    FileWriter,
    runFileWriter,
    writeBinaryFile,
  )
import Effectful.FileSystem.PathReader.Static
  ( getTemporaryDirectory,
    runPathReader,
  )
import Effectful.FileSystem.PathReader.Static qualified as PRD
import Effectful.FileSystem.PathWriter.Static
  ( PathWriter,
    createDirectoryIfMissing,
    removePathForcibly,
    runPathWriter,
  )
import FileSystem.OsPath (OsPath, osp, (</>))
import FileSystem.OsPath qualified as FS.OsPath
import GHC.Stack (HasCallStack)
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
import PathSize.Utils (PosixC, runPosixC)
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
        . runPathSize
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
        . runPathSize
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
        . runPathSize
        . (PathSize.findLargestPaths (baseConfig {strategy}) >=> displayResult)
      where
        desc' = strategyDesc strategy

    displayResult (PathSizeSuccess sbd) = pure $ PathSize.display False sbd
    displayResult (PathSizePartial (err :<|| _) _) = throwM err
    displayResult (PathSizeFailure (err :<|| _)) = throwM err

strategyDesc :: Strategy -> String
strategyDesc Sync = "Sync"
strategyDesc Async = "Async"
strategyDesc AsyncPool = "AsyncPool"

-- | Setups directories for benchmarking.
--
-- @since 0.1
setup :: (HasCallStack) => OsPath -> IO OsPath
setup base = do
  putStrLn "*** Starting setup ***"
  rootDir <- runEff $ runFileWriter $ runPathWriter $ runPathReader $ do
    rootDir <- (</> base) <$> getTemporaryDirectory
    createDirectoryIfMissing False rootDir

    createDenseDirs 11 (rootDir </> [osp|dense-11|]) files100

    pure rootDir

  putStrLn "*** Setup finished ***"
  pure rootDir
  where
    files100 = FS.OsPath.unsafeEncode . show @Int <$> [1 .. 100]

-- | Deletes directories created by 'setup'.
--
-- @since 0.1
teardown :: (HasCallStack) => OsPath -> IO ()
teardown rootDir =
  guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = runEff $ runPathWriter $ removePathForcibly rootDir
    doNothing =
      putStrLn $ "*** Not cleaning up tmp dir: " <> show rootDir

-- | Creates a directory hierarchy of depth 2^n, where each directory contains
-- the parameter files.
createDenseDirs ::
  ( FileWriter :> es,
    HasCallStack,
    PathWriter :> es
  ) =>
  Word8 ->
  OsPath ->
  [OsPath] ->
  Eff es ()
createDenseDirs 0 root paths = createFlatDir root paths
createDenseDirs w root paths = do
  createFlatDir root paths
  traverse_ (\d -> createDenseDirs (w - 1) (root </> d) paths) subDirs
  where
    subDirs = [[osp|d1|], [osp|d2|]]

-- | Creates a single directory with the parameter files.
createFlatDir ::
  ( FileWriter :> es,
    HasCallStack,
    PathWriter :> es
  ) =>
  OsPath ->
  [OsPath] ->
  Eff es ()
createFlatDir root paths = do
  createDirectoryIfMissing False root
  createFiles ((root </>) <$> paths)

-- | Creates empty files at the specified paths.
createFiles :: (FileWriter :> es, HasCallStack) => [OsPath] -> Eff es ()
createFiles = createFileContents . fmap (,"")

-- | Creates files at the specified paths.
createFileContents ::
  (FileWriter :> es, HasCallStack) =>
  [(OsPath, ByteString)] ->
  Eff es ()
createFileContents paths = for_ paths (uncurry writeBinaryFile)

baseConfig :: Config
baseConfig =
  MkConfig
    { -- Want hidden check on even if there are no hidden files, so that we
      -- benchmark the cost.
      searchAll = True,
      maxDepth = Nothing,
      exclude = HSet.empty,
      filesOnly = False,
      ignoreDirIntrinsicSize = False,
      numPaths = Nothing,
      stableSort = False,
      strategy = Sync
    }

runPathSize ::
  Eff
    [ PRD.PathReader,
      PosixC,
      FWD.FileWriter,
      Concurrent,
      IOE
    ]
    a ->
  IO a
runPathSize =
  runEff
    . runConcurrent
    . FWD.runFileWriter
    . runPosixC
    . PRD.runPathReader
