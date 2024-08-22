module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (bracket, displayException, evaluate, throwIO)
import Control.Monad (void)
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import GHC.Conc (setUncaughtExceptionHandler)
import PathSize
  ( PathSizeResult
      ( PathSizeFailure,
        PathSizePartial,
        PathSizeSuccess
      ),
  )
import PathSize qualified
import PathSize.Bench.Common qualified as Common
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

  bracket (Common.setup "profile") Common.teardown $ \testDir -> do
    strategy <-
      getArgs >>= \case
        [] -> pure Sync
        ["sync"] -> pure Sync
        ["async"] -> pure Async
        ["pool"] -> pure AsyncPool
        xs -> die $ "Unexpected args: " ++ show xs

    let config = baseConfig {strategy}

    result <- PathSize.findLargestPaths config testDir
    txt <- displayResult result

    void $ evaluate $ force txt
  where
    displayResult (PathSizeSuccess sbd) = pure $ PathSize.display False sbd
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
