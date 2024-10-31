-- | Main module.
--
-- @since 0.1
module Main (main) where

import Args (Args (path, reverseSort), argsToConfig, getArgs)
import Control.Exception (Exception (displayException))
import Data.Foldable (for_)
import Data.Text qualified as T
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem.PathReader.Static qualified as PR
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import PathSize
  ( PathSizeResult (PathSizeFailure, PathSizePartial, PathSizeSuccess),
    display,
    findLargestPaths,
  )
import PathSize.Utils qualified as Utils

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)

  args <- getArgs
  let config = argsToConfig args
      printResults = putStrLn . T.unpack . display args.reverseSort

  run (findLargestPaths config args.path) >>= \case
    PathSizeSuccess sbd -> printResults sbd
    PathSizePartial errs sbd -> do
      for_ errs (putStrLn . displayException)
      putStrLn ""
      printResults sbd
    PathSizeFailure errs -> for_ errs (putStrLn . displayException)
  where
    run =
      runEff
        . runConcurrent
        . Utils.runPosixC
        . PR.runPathReader
