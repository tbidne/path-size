module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (displayException, evaluate, throwIO)
import Control.Monad (void)
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem.PathReader.Static qualified as PR
import FileSystem.OsPath (encodeValidThrowM)
import GHC.Conc (setUncaughtExceptionHandler)
import PathSize
  ( PathSizeResult
      ( PathSizeFailure,
        PathSizePartial,
        PathSizeSuccess
      ),
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
import PathSize.Utils qualified as Utils
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

  result <- run $ PathSize.findLargestPaths config testPath
  txt <- displayResult result

  void $ evaluate $ force txt
  where
    displayResult (PathSizeSuccess sbd) = pure $ PathSize.display False sbd
    displayResult (PathSizePartial (err :<|| _) _) = throwIO err
    displayResult (PathSizeFailure (err :<|| _)) = throwIO err

    run =
      runEff
        . runConcurrent
        . Utils.runPosixC
        . PR.runPathReader

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
