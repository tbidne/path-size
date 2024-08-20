module GoldenParams
  ( GoldenParams (..),
  )
where

import Data.Text (Text)
import Effects.FileSystem.OsPath (OsPath)
import PathSize.Data.Config (Config)
import Test.Tasty (TestName)

data GoldenParams = MkGoldenParams
  { mConfig :: !(Maybe Config),
    testDesc :: !TestName,
    testName :: !OsPath,
    testPath :: !OsPath,
    runner :: !(Config -> OsPath -> IO Text)
  }
