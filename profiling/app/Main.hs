module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (displayException, evaluate, throwIO)
import Control.Monad (void)
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import FileSystem.OsPath (encodeValidThrowM)
import GHC.Conc (setUncaughtExceptionHandler)
import PathSize
  ( DisplayConfig (color, format),
    DisplayFormat (DisplayFormatSingle),
    PathSizeResult
      ( PathSizeFailure,
        PathSizePartial,
        PathSizeSuccess
      ),
    defaultDisplayConfig,
  )
import PathSize qualified
import PathSize.Data.Config
  ( Config
      ( MkConfig,
        exclude,
        filesOnly,
        ignoreDirIntrinsicSize,
        maxDepth,
        numPaths,
        searchAll,
        stableSort,
        strategy
      ),
    Strategy (Async, AsyncPool, Sync),
  )
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)

  (strategy, testPathStr) <-
    getArgs >>= \case
      ["sync", fp] -> pure (Sync, fp)
      ["async", fp] -> pure (Async, fp)
      ["pool", fp] -> pure (AsyncPool, fp)
      xs -> die $ "Unexpected args: " ++ show xs

  testPath <- encodeValidThrowM testPathStr

  let config = baseConfig {strategy}

  result <- PathSize.findLargestPaths config testPath
  txt <- displayResult result

  void $ evaluate $ force txt
  where
    displayResult (PathSizeSuccess sbd) = pure $ PathSize.display displayConfig sbd
    displayResult (PathSizePartial (err :<|| _) _) = throwIO err
    displayResult (PathSizeFailure (err :<|| _)) = throwIO err

    baseConfig :: Config
    baseConfig =
      MkConfig
        { searchAll = False,
          maxDepth = Nothing,
          exclude = mempty,
          filesOnly = False,
          ignoreDirIntrinsicSize = False,
          numPaths = Nothing,
          stableSort = False,
          strategy = Sync
        }

    displayConfig :: DisplayConfig
    displayConfig =
      defaultDisplayConfig
        { color = False,
          format = DisplayFormatSingle
        }
