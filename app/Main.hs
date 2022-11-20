-- | Main module.
--
-- @since 0.1
module Main (main) where

import Args (getArgs)
import Data.Text qualified as T
import FsSize.PathSize (display, findLargestPaths)
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import GHC.Stack (HasCallStack)
import Optics.Core ((^.))
import UnliftIO.Exception (displayException)

-- | Executable entry-point.
--
-- @since 0.1
main :: HasCallStack => IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)

  args <- getArgs

  result <- findLargestPaths (args ^. #pathSizeConfig) (args ^. #path)

  putStrLn $ T.unpack $ display result
