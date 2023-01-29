module Main (main) where

import Control.Exception.Safe (Exception (displayException), bracket)
import Control.Monad ((>=>))
import Criterion as X
  ( Benchmark,
    bench,
    bgroup,
    nfIO,
  )
import Criterion.Main (defaultMain)
import Data.ByteString (ByteString)
import Data.Foldable (for_, traverse_)
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Data.Word (Word8)
import Effects.Exception
  ( HasCallStack,
    addCS,
    throwCS,
  )
import Effects.FileSystem.FileWriter (MonadFileWriter (..))
import Effects.FileSystem.PathReader (MonadPathReader (..))
import Effects.FileSystem.PathWriter (MonadPathWriter (..))
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import Numeric.Data.Positive (mkPositive)
import PathSize (display, findLargestPaths)
import PathSize.Data.Config
  ( Config (numPaths, strategy),
    Strategy (Async, AsyncPooled, Sync),
  )
import PathSize.Data.PathSizeResult (PathSizeResult (..))
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import System.FilePath ((</>))

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
    "findLargestPaths All"
    [ flat,
      sparse,
      dense
    ]
  where
    flat =
      bgroup
        "flat"
        [ findLargestSync "100 - Sync" (testDir </> "flat-100"),
          findLargestAsync "100 - Async" (testDir </> "flat-100"),
          findLargestAsyncPooled "100 - AsyncPooled" (testDir </> "flat-100"),
          findLargestSync "1,000 - Sync" (testDir </> "flat-1_000"),
          findLargestAsync "1,000 - Async" (testDir </> "flat-1_000"),
          findLargestAsyncPooled "1,000 - AsyncPooled" (testDir </> "flat-1_000"),
          findLargestSync "10,000 - Sync" (testDir </> "flat-10_000"),
          findLargestAsync "10,000 - Async" (testDir </> "flat-10_000"),
          findLargestAsyncPooled "10,000 - AsyncPooled" (testDir </> "flat-10_000")
        ]
    sparse =
      bgroup
        "sparse"
        [ findLargestSync "d = 6 - Sync" (testDir </> "sparse-6"),
          findLargestAsync "d = 6 - Async" (testDir </> "sparse-6"),
          findLargestAsyncPooled "d = 6 - AsyncPooled" (testDir </> "sparse-6"),
          findLargestSync "d = 8 - Sync" (testDir </> "sparse-8"),
          findLargestAsync "d = 8 - Async" (testDir </> "sparse-8"),
          findLargestAsyncPooled "d = 8 - AsyncPooled" (testDir </> "sparse-8"),
          findLargestSync "d = 10 - Sync" (testDir </> "sparse-10"),
          findLargestAsync "d = 10 - Async" (testDir </> "sparse-10"),
          findLargestAsyncPooled "d = 10 - AsyncPooled" (testDir </> "sparse-10")
        ]
    dense =
      bgroup
        "dense"
        [ findLargestSync "d = 6 - Sync" (testDir </> "dense-6"),
          findLargestAsync "d = 6 - Async" (testDir </> "dense-6"),
          findLargestAsyncPooled "d = 6 - AsyncPooled" (testDir </> "dense-6"),
          findLargestSync "d = 8 - Sync" (testDir </> "dense-8"),
          findLargestAsync "d = 8 - Async" (testDir </> "dense-8"),
          findLargestAsyncPooled "d = 8 - AsyncPooled" (testDir </> "dense-8"),
          findLargestSync "d = 10 - Sync" (testDir </> "dense-10"),
          findLargestAsync "d = 10 - Async" (testDir </> "dense-10"),
          findLargestAsyncPooled "d = 10 - AsyncPooled" (testDir </> "dense-10")
        ]
    findLargestSync desc =
      bench desc
        . nfIO
        . findLargestPaths mempty {strategy = Sync}
    findLargestAsync desc =
      bench desc
        . nfIO
        . findLargestPaths mempty {strategy = Async}
    findLargestAsyncPooled desc =
      bench desc
        . nfIO
        . findLargestPaths mempty {strategy = AsyncPooled}

benchLargestN :: FilePath -> Benchmark
benchLargestN testDir =
  bgroup
    "takeLargestN"
    [ runLargestN "1" (mkPositive 1) (testDir </> "dense-10"),
      runLargestN "10" (mkPositive 10) (testDir </> "dense-10"),
      runLargestN "100" (mkPositive 100) (testDir </> "dense-10"),
      runLargestN "1,000" (mkPositive 1_000) (testDir </> "dense-10")
    ]
  where
    runLargestN desc n =
      bench desc
        . nfIO
        . findLargestPaths (mempty {numPaths = n})

benchDisplayPathSize :: FilePath -> Benchmark
benchDisplayPathSize testDir =
  bgroup
    "displayPathSize"
    [ runDisplayPathSize "d = 6" (testDir </> "dense-6"),
      runDisplayPathSize "d = 8" (testDir </> "dense-8"),
      runDisplayPathSize "d = 10" (testDir </> "dense-10")
    ]
  where
    runDisplayPathSize desc =
      bench desc
        . nfIO
        . (findLargestPaths mempty >=> displayResult)
    displayResult (PathSizeSuccess sbd) = pure $ display False sbd
    displayResult (PathSizePartial (err :<|| _) _) = throwCS err

setup :: HasCallStack => IO FilePath
setup = do
  rootDir <- (</> "bench") <$> getTemporaryDirectory
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

teardown :: HasCallStack => FilePath -> IO ()
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
createFlatDir :: HasCallStack => FilePath -> [FilePath] -> IO ()
createFlatDir root paths = do
  createDirectoryIfMissing False root
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
  \(p, c) -> addCS $ writeBinaryFile p c
