module Main (main) where

import Control.Exception (Exception (displayException), bracket)
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import PathSize.Bench.Common (BenchmarkSuite (MkBenchmarkSuite, bench, bgroup, nfIO))
import PathSize.Bench.Common qualified as Common
import PathSize.Data.Config (Strategy (Sync))
import Test.Tasty.Bench (Benchmark, Benchmarkable, defaultMain)
import Test.Tasty.Bench qualified as Bench

main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)

  bracket (Common.setup "tasty-bench") Common.teardown runBenchmarks
  where
    runBenchmarks testDir =
      defaultMain
        [ Common.benchPathSizeRecursive suite syncNE testDir,
          Common.benchLargest10 suite syncNE testDir,
          Common.benchDisplayPathSize suite syncNE testDir
        ]
    syncNE = Sync :| []

suite :: BenchmarkSuite Benchmarkable Benchmark
suite =
  MkBenchmarkSuite
    { bgroup = Bench.bgroup,
      bench = Bench.bench,
      nfIO = Bench.nfIO
    }
