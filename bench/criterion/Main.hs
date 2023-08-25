module Main (main) where

import Control.Concurrent (getNumCapabilities)
import Criterion qualified as Bench
import Criterion.Main (Benchmark, Benchmarkable, defaultMain)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Effects.Exception (Exception (displayException), bracket)
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import PathSize.Data.Config (Strategy (Async, AsyncPool, Sync))
import SafeRm.Bench.Common (BenchmarkSuite (MkBenchmarkSuite, bench, bgroup, nfIO))
import SafeRm.Bench.Common qualified as Common

main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)
  numCap <- getNumCapabilities
  putStrLn $ "*** Running with " ++ show numCap ++ " capabilities ***"

  bracket (Common.setup "criterion") Common.teardown runBenchmarks
  where
    runBenchmarks testDir =
      defaultMain
        [ integerBench testDir,
          intBench testDir
        ]

    integerBench testDir =
      Bench.bgroup
        "integer"
        [ Common.benchPathSizeRecursive @_ @_ @Integer suite syncNE testDir,
          Common.benchLargest10 @_ @_ @Integer suite syncNE testDir,
          Common.benchDisplayPathSize @_ @_ @Integer suite syncNE testDir
        ]
    intBench testDir =
      Bench.bgroup
        "int"
        [ Common.benchPathSizeRecursive @_ @_ @Int suite syncNE testDir,
          Common.benchLargest10 @_ @_ @Int suite syncNE testDir,
          Common.benchDisplayPathSize @_ @_ @Int suite syncNE testDir
        ]

    syncNE = Sync :| [Async, AsyncPool]

suite :: BenchmarkSuite Benchmarkable Benchmark
suite =
  MkBenchmarkSuite
    { bgroup = Bench.bgroup,
      bench = Bench.bench,
      nfIO = Bench.nfIO
    }
