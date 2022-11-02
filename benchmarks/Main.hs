module Main (main) where

import Criterion as X
  ( Benchmark,
    bench,
    bgroup,
    nfIO,
  )
import Criterion.Main (defaultMain)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (for_, traverse_)
import Data.Word (Word8)
import FsUtils.Control.Exception (withCallStack)
import FsUtils.Data.PathSize (displayPathSize, largestN)
import FsUtils.Size
  ( pathSizeRecursive,
    pathSizeRecursiveAsync,
    pathSizeRecursiveParallel,
  )
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import GHC.Stack (HasCallStack)
import System.Directory qualified as Dir
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import System.FilePath ((</>))
import UnliftIO.Exception (Exception (displayException), bracket)

main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)

  bracket setup teardown runBenchmarks
  where
    runBenchmarks testDir =
      defaultMain
        [ benchPathSizeRecursive testDir,
          benchLargestN testDir,
          benchDisplayPathSize testDir
        ]

benchPathSizeRecursive :: FilePath -> Benchmark
benchPathSizeRecursive testDir =
  bgroup
    "pathSizeRecursive"
    [ flat,
      sparse,
      dense
    ]
  where
    flat =
      bgroup
        "flat"
        [ runPathSizeRecursive "100" (testDir </> "flat-100"),
          runPathSizeRecursiveAsync "100 - Async" (testDir </> "flat-100"),
          runPathSizeRecursiveParallel "100 - Parallel" (testDir </> "flat-100"),
          runPathSizeRecursive "1,000" (testDir </> "flat-1_000"),
          runPathSizeRecursiveAsync "1,000 - Async" (testDir </> "flat-1_000"),
          runPathSizeRecursiveParallel "1,000 - Parallel" (testDir </> "flat-1_000"),
          runPathSizeRecursive "10,000" (testDir </> "flat-10_000"),
          runPathSizeRecursiveAsync "10,000 - Async" (testDir </> "flat-10_000"),
          runPathSizeRecursiveParallel "10,000 - Parallel" (testDir </> "flat-10_000")
        ]
    sparse =
      bgroup
        "sparse"
        [ runPathSizeRecursive "d = 6" (testDir </> "sparse-6"),
          runPathSizeRecursiveAsync "d = 6 - Async" (testDir </> "sparse-6"),
          runPathSizeRecursiveParallel "d = 6 - Parallel" (testDir </> "sparse-6"),
          runPathSizeRecursive "d = 8" (testDir </> "sparse-8"),
          runPathSizeRecursiveAsync "d = 8 - Async" (testDir </> "sparse-8"),
          runPathSizeRecursiveParallel "d = 8 - Parallel" (testDir </> "sparse-8"),
          runPathSizeRecursive "d = 10" (testDir </> "sparse-10"),
          runPathSizeRecursiveAsync "d = 10 - Async" (testDir </> "sparse-10"),
          runPathSizeRecursiveParallel "d = 10 - Parallel" (testDir </> "sparse-10")
        ]
    dense =
      bgroup
        "dense"
        [ runPathSizeRecursive "d = 6" (testDir </> "dense-6"),
          runPathSizeRecursiveAsync "d = 6 - Async" (testDir </> "dense-6"),
          runPathSizeRecursiveParallel "d = 6 - Parallel" (testDir </> "dense-6"),
          runPathSizeRecursive "d = 8" (testDir </> "dense-8"),
          runPathSizeRecursiveAsync "d = 8 - Async" (testDir </> "dense-8"),
          runPathSizeRecursiveParallel "d = 8 - Parallel" (testDir </> "dense-8"),
          runPathSizeRecursive "d = 10" (testDir </> "dense-10"),
          runPathSizeRecursiveAsync "d = 10 - Async" (testDir </> "dense-10"),
          runPathSizeRecursiveParallel "d = 10 - Parallel" (testDir </> "dense-10")
        ]
    runPathSizeRecursive desc = bench desc . nfIO . pathSizeRecursive
    runPathSizeRecursiveAsync desc = bench desc . nfIO . pathSizeRecursiveAsync
    runPathSizeRecursiveParallel desc = bench desc . nfIO . pathSizeRecursiveParallel

benchLargestN :: FilePath -> Benchmark
benchLargestN testDir =
  bgroup
    "largestN"
    [ runLargestN "1" (testDir </> "dense-10") 1,
      runLargestN "10" (testDir </> "dense-10") 10,
      runLargestN "100" (testDir </> "dense-10") 100,
      runLargestN "1,000" (testDir </> "dense-10") 1_000
    ]
  where
    runLargestN desc path n = bench desc $ nfIO $ do
      mp <- pathSizeRecursive path
      pure $ largestN n mp

benchDisplayPathSize :: FilePath -> Benchmark
benchDisplayPathSize testDir =
  bgroup
    "displayPathSize"
    [ runDisplayPathSize "d = 6" (testDir </> "dense-6"),
      runDisplayPathSize "d = 8" (testDir </> "dense-8"),
      runDisplayPathSize "d = 10" (testDir </> "dense-10")
    ]
  where
    runDisplayPathSize desc path = bench desc $ nfIO $ do
      mp <- pathSizeRecursive path
      pure $ displayPathSize mp

setup :: HasCallStack => IO FilePath
setup = do
  rootDir <- (</> "bench") <$> Dir.getTemporaryDirectory
  Dir.createDirectoryIfMissing False rootDir

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

teardown :: HasCallStack => FilePath -> IO ()
teardown rootDir =
  withCallStack $
    guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    -- rootDir = args ^. #rootDir
    cleanup = Dir.removePathForcibly rootDir
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
createFlatDir :: HasCallStack => FilePath -> [FilePath] -> IO ()
createFlatDir root paths = do
  Dir.createDirectoryIfMissing False root
  createFiles ((root </>) <$> paths)

-- | Creates empty files at the specified paths.
createFiles :: HasCallStack => [FilePath] -> IO ()
createFiles = createFileContents . fmap (,"")

-- | Creates files at the specified paths.
createFileContents ::
  HasCallStack =>
  [(FilePath, ByteString)] ->
  IO ()
createFileContents paths = for_ paths $
  \(p, c) -> withCallStack $ BS.writeFile p c
