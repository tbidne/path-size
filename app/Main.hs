-- | Main module.
--
-- @since 0.1
module Main (main) where

import Args (argsToConfig, getArgs)
import Data.Foldable (for_)
import Data.Text qualified as T
import Effectful (Eff, IOE, runEff)
import Effectful.CallStack (CallStackEffect, displayCallStack, runCallStackIO)
import Effectful.Concurrent.Async (Concurrent, runConcurrent)
import Effectful.FileSystem.PathReader (PathReaderEffect, runPathReaderIO)
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import Optics.Core ((^.))
import PathSize (PathSizeEffect, PathSizeResult (..), display, findLargestPathsIO, runPathSizeIO)
import System.Exit (exitFailure)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayCallStack)

  args <- getArgs
  let config = argsToConfig args
      printResults = putStrLn . T.unpack . display (args ^. #reverseSort)

  runPathSize (findLargestPathsIO config (args ^. #path)) >>= \case
    PathSizeSuccess sbd -> printResults sbd
    PathSizePartial errs sbd -> do
      for_ errs (putStrLn . displayCallStack)
      printResults sbd
    PathSizeFailure errs -> do
      for_ errs (putStrLn . displayCallStack)
      exitFailure
  where
    runPathSize ::
      Eff
        '[ PathSizeEffect,
           PathReaderEffect,
           CallStackEffect,
           Concurrent,
           IOE
         ]
        a ->
      IO a
    runPathSize =
      runEff
        . runConcurrent
        . runCallStackIO
        . runPathReaderIO
        . runPathSizeIO
