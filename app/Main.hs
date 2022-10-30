-- | Main module.
--
-- @since 0.1
module Main (main) where

import Control.Exception.Safe (Exception (displayException), throwString)
import Data.Foldable (Foldable (foldl'))
import Data.HashMap.Strict qualified as HMap
import Data.List qualified as L
import Data.Ord (Down (Down))
import FsUtils.Size (Path, pathSizeRecursive)
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import GHC.Stack (HasCallStack)
import System.Environment (getArgs)

-- | Executable entry-point.
--
-- @since 0.1
main :: HasCallStack => IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)

  path <-
    getArgs >>= \case
      [path] -> pure path
      other ->
        throwString $
          "Expected a single path, received "
            <> show other

  sizeMap <- pathSizeRecursive path
  let sizeList = HMap.toList sizeMap
      sorted = L.sortOn (Down . snd) sizeList

  putStrLn $ showList' sorted
  print $ length sorted
  where
    -- TODO: Text Builder
    showList' :: [(Path, Integer)] -> String
    showList' = foldl' go ""
    go acc (path, size) =
      (show path <> ": " <> show size <> "\n") <> acc
