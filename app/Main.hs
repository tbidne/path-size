-- | Main module.
--
-- @since 0.1
module Main (main) where

import Args (getArgs)
import Data.Text qualified as T
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import GHC.Stack (HasCallStack)
import Optics.Core ((^.))
import PathSize (display, findLargestPaths)
import UnliftIO.Exception (displayException)

-- | Executable entry-point.
--
-- @since 0.1
main :: HasCallStack => IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)

  args <- getArgs
  let config = args ^. #config

  result <- findLargestPaths config (args ^. #path)

  putStrLn $ T.unpack $ display (config ^. #reverseSort) result
