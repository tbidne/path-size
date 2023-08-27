{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

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
import Data.ByteString (ByteString)
import Data.Foldable (for_, traverse_)
import Data.HashSet qualified as HSet
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Data.Word (Word8)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent qualified as CC
import Effectful.Exception (throwM)
import Effectful.FileSystem.FileWriter.Static (FileWriterStatic)
import Effectful.FileSystem.FileWriter.Static qualified as FW
import Effectful.FileSystem.PathReader.Dynamic (PathReaderDynamic)
import Effectful.FileSystem.PathReader.Dynamic qualified as PRD
import Effectful.FileSystem.PathReader.Static (PathReaderStatic)
import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.PathWriter.Static (PathWriterStatic)
import Effectful.FileSystem.PathWriter.Static qualified as PW
import Effectful.FileSystem.Utils (OsPath, osp, (</>), (</>!))
import Effectful.FileSystem.Utils qualified as FsUtils
import Effectful.PosixCompat.Static (PosixCompatStatic)
import Effectful.PosixCompat.Static qualified as Posix
import Effectful.Terminal.Static (TerminalStatic)
import Effectful.Terminal.Static qualified as Term
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
import System.Environment.Guard.Lifted (ExpectEnv (ExpectEnvSet), guardOrElse')

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

runPathSize ::
  Eff
    [ PosixCompatStatic,
      Concurrent,
      PathReaderDynamic,
      IOE
    ]
    a ->
  IO a
runPathSize =
  runEff
    . PRD.runPathReaderDynamicIO
    . CC.runConcurrent
    . Posix.runPosixCompatStaticIO

strategyDesc :: Strategy -> String
strategyDesc Sync = "Sync"
strategyDesc Async = "Async"
strategyDesc AsyncPool = "AsyncPool"

-- | Setups directories for benchmarking.
--
-- @since 0.1
setup :: FilePath -> IO OsPath
setup base = runSetup $ do
  Term.putStrLn "*** Starting setup ***"
  rootDir <- (</>! base) <$> PR.getTemporaryDirectory
  PW.createDirectoryIfMissing False rootDir

  createDenseDirs 11 (rootDir </> [osp|dense-11|]) files100

  Term.putStrLn "*** Setup finished ***"
  pure rootDir
  where
    files100 = FsUtils.unsafeEncodeFpToOs . show @Int <$> [1 .. 100]
    runSetup ::
      Eff
        [ FileWriterStatic,
          PathReaderStatic,
          PathWriterStatic,
          TerminalStatic,
          IOE
        ]
        a ->
      IO a
    runSetup =
      runEff
        . Term.runTerminalStaticIO
        . PW.runPathWriterStaticIO
        . PR.runPathReaderStaticIO
        . FW.runFileWriterStaticIO

-- | Deletes directories created by 'setup'.
--
-- @since 0.1
teardown :: OsPath -> IO ()
teardown rootDir =
  runTeardown $
    guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = PW.removePathForcibly rootDir
    doNothing =
      Term.putStrLn $ "*** Not cleaning up tmp dir: " <> show rootDir

    runTeardown =
      runEff
        . PW.runPathWriterStaticIO
        . Term.runTerminalStaticIO

-- | Creates a directory hierarchy of depth 2^n, where each directory contains
-- the parameter files.
createDenseDirs ::
  ( FileWriterStatic :> es,
    PathWriterStatic :> es
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
  ( FileWriterStatic :> es,
    PathWriterStatic :> es
  ) =>
  OsPath ->
  [OsPath] ->
  Eff es ()
createFlatDir root paths = do
  PW.createDirectoryIfMissing False root
  createFiles ((root </>) <$> paths)

-- | Creates empty files at the specified paths.
createFiles :: (FileWriterStatic :> es) => [OsPath] -> Eff es ()
createFiles = createFileContents . fmap (,"")

-- | Creates files at the specified paths.
createFileContents ::
  (FileWriterStatic :> es) =>
  [(OsPath, ByteString)] ->
  Eff es ()
createFileContents paths =
  for_ paths $
    uncurry FW.writeBinaryFile

baseConfig :: Config
baseConfig =
  MkConfig
    { searchAll = False,
      maxDepth = Nothing,
      exclude = HSet.empty,
      filesOnly = False,
      ignoreDirIntrinsicSize = False,
      numPaths = Nothing,
      stableSort = False,
      strategy = Sync
    }
