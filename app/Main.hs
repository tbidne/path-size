-- | Main module.
--
-- @since 0.1
module Main (main) where

import Args (argsToConfig, getArgs)
import Control.Exception (Exception (displayException))
import Data.Foldable (for_)
import Data.Text qualified as T
import Effects.MonadCallStack (displayCallStack)
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import Optics.Core ((^.))
import PathSize (PathSizeResult (..), display, findLargestPaths)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayCallStack)

  args <- getArgs
  let config = argsToConfig args
      printResults = putStrLn . T.unpack . display (args ^. #reverseSort)

  findLargestPaths config (args ^. #path) >>= \case
    PathSizeSuccess sbd -> printResults sbd
    PathSizePartial errs sbd -> do
      for_ errs (putStrLn . displayException)
      putStrLn ""
      printResults sbd
