-- | Entry point for functional tests.
--
-- @since 0.1
module Main (main) where

import Functional.FsUtils.PathSize qualified as FsUtils.PathSize
import Test.Tasty qualified as Tasty

-- | Runs functional tests.
--
-- @since 0.1
main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup
      "Functional Tests"
      [ FsUtils.PathSize.tests
      ]
