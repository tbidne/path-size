-- | Entry point for functional tests.
--
-- @since 0.1
module Main (main) where

import Functional.PathSize qualified as PathSize
import Test.Tasty qualified as Tasty
import Test.Tasty.Golden (DeleteOutputFile (OnPass))

-- | Runs functional tests.
--
-- @since 0.1
main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.localOption OnPass $
      Tasty.testGroup
        "Functional Tests"
        [ PathSize.tests
        ]
