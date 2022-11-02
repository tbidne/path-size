-- | Main module.
--
-- @since 0.1
module Main (main) where

import FsUtils.Size
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import GHC.Stack (HasCallStack)
import System.Environment (getArgs)
import UnliftIO.Exception (displayException, throwString)

-- | Executable entry-point.
--
-- @since 0.1
main :: HasCallStack => IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)

  result <-
    getArgs >>= \case
      [path] -> pathSizeRecursive path
      [path, "c"] -> pathSizeRecursiveAsync path
      [path, "p"] -> pathSizeRecursiveParallel path
      other ->
        throwString $ "Unexpected args " <> show other

  -- let sorted = sortPathSize sizeMap
  print $ length result
