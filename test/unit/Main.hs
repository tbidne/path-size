-- | Entry point for unit tests.
--
-- @since 0.1
module Main (main) where

import Test.Tasty qualified as Tasty
import Unit.PathSize.Utils qualified as PathSize.Utils

-- | Runs functional tests.
--
-- @since 0.1
main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup
      "Unit Tests"
      [ PathSize.Utils.tests
      ]
