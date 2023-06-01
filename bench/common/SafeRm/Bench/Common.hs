{-# LANGUAGE RecordWildCards #-}

-- | Provides common benchmarking functionality.
--
-- @since 0.1
module SafeRm.Bench.Common
  ( -- Benchmarks
    BenchmarkSuite (..),
    benchPathSizeRecursive,
    benchLargestN,
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
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Data.Word (Word8)
import Effects.Exception (HasCallStack, addCS, throwCS)
import Effects.FileSystem.FileWriter (ByteString, writeBinaryFile)
import Effects.FileSystem.Path (Path, (</>))
import Effects.FileSystem.PathReader (getTemporaryDirectory)
import Effects.FileSystem.PathWriter
  ( createDirectoryIfMissing,
    removePathForcibly,
  )
import Numeric.Data.Positive (mkPositive)
import PathSize
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
  -- | Benchmark strategies. We run each strategy over 3 levels for each of
  -- flat/sparse/dense directories i.e. total benchmarks is
  -- @3 * 3 * |strategies|@.
  [Strategy] ->
  -- | Test directory home.
  FilePath ->
  b
benchPathSizeRecursive MkBenchmarkSuite {..} strategies testDir =
  bgroup
    "findLargestPaths All"
    [ flat,
      sparse,
      dense
    ]
  where
    flat =
      bgroup
        "flat"
        [ findLargest s desc dir
          | (desc, dir) <- flatData,
            s <- strategies
        ]
    flatData =
      [ ("100", testDir </> "flat-100"),
        ("1_000", testDir </> "flat-1_000"),
        ("10_000", testDir </> "flat-10_000")
      ]

    sparse =
      bgroup
        "sparse"
        [ findLargest s desc dir
          | (desc, dir) <- sparseData,
            s <- strategies
        ]
    sparseData =
      [ ("d=6", testDir </> "sparse-6"),
        ("d=8", testDir </> "sparse-8"),
        ("d=10", testDir </> "sparse-10")
      ]

    dense =
      bgroup
        "dense"
        [ findLargest s desc dir
          | (desc, dir) <- denseData,
            s <- strategies
        ]
    denseData =
      [ ("d=6", testDir </> "dense-6"),
        ("d=8", testDir </> "dense-8"),
        ("d=10", testDir </> "dense-10")
      ]

    findLargest :: Strategy -> String -> Path -> b
    findLargest strategy desc =
      bench desc'
        . nfIO
        . findLargestPaths baseConfig {strategy}
      where
        desc' =
          desc <> case strategy of
            Sync -> " - Sync"
            Async -> " - Async"
            AsyncPooled -> " - AsyncPooled"

-- | Benchmark for finding the largest N files in a dense directory tree.
--
-- @since 0.1
benchLargestN :: BenchmarkSuite f b -> FilePath -> b
benchLargestN MkBenchmarkSuite {..} testDir =
  bgroup
    "takeLargestN"
    [ runLargestN "1" (mkPositive 1) (testDir </> "dense-10"),
      runLargestN "10" (mkPositive 10) (testDir </> "dense-10"),
      runLargestN "100" (mkPositive 100) (testDir </> "dense-10"),
      runLargestN "1_000" (mkPositive 1_000) (testDir </> "dense-10")
    ]
  where
    runLargestN desc n =
      bench desc
        . nfIO
        . findLargestPaths (baseConfig {numPaths = n})

-- | Benchmark for displaying results of a path search.
--
-- @since 0.1
benchDisplayPathSize :: BenchmarkSuite f b -> FilePath -> b
benchDisplayPathSize MkBenchmarkSuite {..} testDir =
  bgroup
    "displayPathSize"
    [ runDisplayPathSize "d=6" (testDir </> "dense-6"),
      runDisplayPathSize "d=8" (testDir </> "dense-8"),
      runDisplayPathSize "d=10" (testDir </> "dense-10")
    ]
  where
    runDisplayPathSize desc =
      bench desc
        . nfIO
        . (findLargestPaths baseConfig >=> displayResult)
    displayResult (PathSizeSuccess sbd) = pure $ display False sbd
    displayResult (PathSizePartial (err :<|| _) _) = throwCS err
    displayResult (PathSizeFailure (err :<|| _)) = throwCS err

-- | Setups directories for benchmarking.
--
-- @since 0.1
setup :: (HasCallStack) => FilePath -> IO FilePath
setup base = do
  rootDir <- (</> base) <$> getTemporaryDirectory
  createDirectoryIfMissing False rootDir

  -- flat directories
  createFlatDir (rootDir </> "flat-100") files100
  createFlatDir (rootDir </> "flat-1_000") files1000
  createFlatDir (rootDir </> "flat-10_000") files10000

  -- spare directory hierarchy
  createSpareDirs 6 (rootDir </> "sparse-6") files100
  createSpareDirs 8 (rootDir </> "sparse-8") files100
  createSpareDirs 10 (rootDir </> "sparse-10") files100

  -- dense directory hierarchy
  createDenseDirs 6 (rootDir </> "dense-6") files100
  createDenseDirs 8 (rootDir </> "dense-8") files100
  createDenseDirs 10 (rootDir </> "dense-10") files100
  pure rootDir
  where
    files100 = show @Int <$> [1 .. 100]
    files1000 = files100 <> (show @Int <$> [11 .. 1_000])
    files10000 = files1000 <> (show @Int <$> [1_001 .. 10_000])

-- | Deletes directories created by 'setup'.
--
-- @since 0.1
teardown :: (HasCallStack) => FilePath -> IO ()
teardown rootDir =
  addCS $
    guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = removePathForcibly rootDir
    doNothing =
      putStrLn $ "*** Not cleaning up tmp dir: " <> rootDir

-- | Creates a directory hierarchy of depth 2^n, where each directory contains
-- the parameter files.
createDenseDirs :: Word8 -> FilePath -> [FilePath] -> IO ()
createDenseDirs 0 root paths = createFlatDir root paths
createDenseDirs w root paths = do
  createFlatDir root paths
  traverse_ (\d -> createDenseDirs (w - 1) (root </> d) paths) subDirs
  where
    subDirs = ["d1", "d2"]

-- | Creates a directory hierarchy of depth 2^n, where the "leaf" directories
-- contain the parameter files.
createSpareDirs :: Word8 -> FilePath -> [FilePath] -> IO ()
createSpareDirs 0 root paths = createFlatDir root paths
createSpareDirs w root paths = do
  createFlatDir root []
  traverse_ (\d -> createSpareDirs (w - 1) (root </> d) paths) subDirs
  where
    subDirs = ["d1", "d2"]

-- | Creates a single directory with the parameter files.
createFlatDir :: (HasCallStack) => FilePath -> [FilePath] -> IO ()
createFlatDir root paths = do
  createDirectoryIfMissing False root
  createFiles ((root </>) <$> paths)

-- | Creates empty files at the specified paths.
createFiles :: (HasCallStack) => [FilePath] -> IO ()
createFiles = createFileContents . fmap (,"")

-- | Creates files at the specified paths.
createFileContents ::
  (HasCallStack) =>
  [(FilePath, ByteString)] ->
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
      strategy = Sync
    }
