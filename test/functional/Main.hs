-- | Entry point for functional tests.
--
-- @since 0.1
module Main (main) where

import Functional.PathSize qualified as PathSize
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty qualified as Tasty

-- | Runs functional tests.
--
-- @since 0.1
main :: IO ()
main =
  -- FIXME: Remove non-determinism so we can always run these.
  guardOrElse' "RUN_FUNCTIONAL" ExpectEnvSet runTests printWarning
  where
    runTests =
      Tasty.defaultMain $
        Tasty.testGroup
          "Functional Tests"
          [ PathSize.tests
          ]
    printWarning =
      putStrLn $
        mconcat
          [ "*** Non-deterministic functional tests disabled. Enable ",
            "with RUN_FUNCTIONAL=1 ***"
          ]
