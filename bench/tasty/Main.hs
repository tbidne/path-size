module Main (main) where

import Effects.Exception (Exception (displayException), bracket)
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import PathSize.Data.Config (Strategy (Sync))
import SafeRm.Bench.Common (BenchmarkSuite (MkBenchmarkSuite, bench, bgroup, nfIO))
import SafeRm.Bench.Common qualified as Common
import Test.Tasty.Bench (Benchmark, Benchmarkable, defaultMain)
import Test.Tasty.Bench qualified as Bench

main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)

  bracket (Common.setup "tasty-bench") Common.teardown runBenchmarks
  where
    runBenchmarks testDir =
      defaultMain
        [ Common.benchPathSizeRecursive suite [Sync] testDir,
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
