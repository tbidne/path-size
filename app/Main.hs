-- | Main module.
--
-- @since 0.1
module Main (main) where

import FsUtils.PathSize
  ( PathSizeConfig (strategy),
    Strategy (Async, AsyncPooled, Sync),
    findLargestPaths,
  )
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
      [path] -> findLargestPaths (mempty {strategy = Sync}) path
      [path, "c"] -> findLargestPaths (mempty {strategy = Async}) path
      [path, "p"] -> findLargestPaths (mempty {strategy = AsyncPooled}) path
      other ->
        throwString $ "Unexpected args " <> show other

  -- let sorted = sort sizeMap
  print $ length result
