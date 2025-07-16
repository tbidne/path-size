-- | Entry point for functional tests.
--
-- @since 0.1
module Main (main) where

import Functional.PathSize qualified as PathSize
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty qualified as Tasty
import Test.Tasty.Golden (DeleteOutputFile (OnPass))

-- | Runs functional tests.
--
-- @since 0.1
main :: IO ()
main = guardOrElse' "RUN_FUNC" ExpectEnvSet runTests doNothing
  where
    runTests = do
      Tasty.defaultMain $
        Tasty.localOption OnPass $
          Tasty.testGroup
            "Functional Tests"
            [ PathSize.tests
            ]

    doNothing =
      putStrLn "*** Functional tests disabled. Enabled with RUN_FUNC=1 ***"
