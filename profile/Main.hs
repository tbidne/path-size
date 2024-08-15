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

-- Profiling doesn't show a lot of low-hanging fruit. One of the more
-- expensive operations appears to be decodeOsToFp, which we call since
-- we need to convert our OsPaths into FilePaths so that they can be used
-- with getPathType, which uses unix-compat's getSymbolicLinkStatus.
-- There are a couple options:
--
-- 1. Downgrade to FilePath.
--
--    - I really don't want to do this as it the OsPath type is much better.
--
-- 2. Somehow skip the conversion.
--
--   - This may actually be possible because the unix package offers the
--     same function in terms of PosixPath, which is what OsPath is under the
--     hood (on unix). So theoretically we could use this function directly.
--
--     The main annoyance is that getPathType comes from monad-effects, so
--     we'd need to make some changes there. This definitely seems worth doing,
--     but it's not entirely straightforward. We'd need to depend on unix
--     conditionally with CPP, and use those functions when available.
--     Otherwise use unix-compat. We'd want the type signatures to align,
--     however, so that may mean conversions from OsPath to FilePath.

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
