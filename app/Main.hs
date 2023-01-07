-- | Main module.
--
-- @since 0.1
module Main (main) where

import Args (argsToConfig, getArgs)
import Data.Foldable (for_)
import Data.Text qualified as T
import Effects.MonadCallStack (displayCallStack)
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import GHC.Stack (HasCallStack)
import Optics.Core ((^.))
import PathSize (PathSizeResult (..), display, findLargestPaths)
import System.Exit (exitFailure)

-- | Executable entry-point.
--
-- @since 0.1
main :: HasCallStack => IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayCallStack)

  args <- getArgs
  let config = argsToConfig args
      printResults = putStrLn . T.unpack . display (args ^. #reverseSort)

  findLargestPaths config (args ^. #path) >>= \case
    PathSizeSuccess sbd -> printResults sbd
    PathSizePartial errs sbd -> do
      for_ errs (putStrLn . displayCallStack)
      printResults sbd
    PathSizeFailure errs -> do
      for_ errs (putStrLn . displayCallStack)
      exitFailure
