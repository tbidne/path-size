module Main (main) where

import Control.Concurrent (getNumCapabilities)
import Criterion qualified as Bench
import Criterion.Main (Benchmark, Benchmarkable, defaultMain)
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
        [ Common.benchPathSizeRecursive suite [Sync, Async, AsyncPool] testDir,
          Common.benchLargestN suite testDir,
          Common.benchDisplayPathSize suite testDir
        ]

suite :: BenchmarkSuite Benchmarkable Benchmark
suite =
  MkBenchmarkSuite
    { bgroup = Bench.bgroup,
      bench = Bench.bench,
      nfIO = Bench.nfIO
    }
