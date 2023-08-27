module Main (main) where

import Control.Concurrent (getNumCapabilities)
import Criterion qualified as Bench
import Criterion.Main (Benchmark, Benchmarkable, defaultMain)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Effectful.Exception (Exception (displayException), bracket)
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import PathSize.Bench.Common (BenchmarkSuite (MkBenchmarkSuite, bench, bgroup, nfIO))
import PathSize.Bench.Common qualified as Common
import PathSize.Data.Config (Strategy (Async, AsyncPool, Sync))

main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)
  numCap <- getNumCapabilities
  putStrLn $ "*** Running with " ++ show numCap ++ " capabilities ***"

  bracket (Common.setup "criterion") Common.teardown runBenchmarks
  where
    runBenchmarks testDir =
      defaultMain
        [ Common.benchPathSizeRecursive suite syncNE testDir,
          Common.benchLargest10 suite syncNE testDir,
          Common.benchDisplayPathSize suite syncNE testDir
        ]
    syncNE = Sync :| [Async, AsyncPool]

suite :: BenchmarkSuite Benchmarkable Benchmark
suite =
  MkBenchmarkSuite
    { bgroup = Bench.bgroup,
      bench = Bench.bench,
      nfIO = Bench.nfIO
    }
