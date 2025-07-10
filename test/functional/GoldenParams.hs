module GoldenParams
  ( GoldenParams (..),
  )
where

import Data.Text (Text)
import FileSystem.OsPath (OsPath)
import PathSize (DisplayConfig)
import PathSize.Data.Config (Config)
import Test.Tasty (TestName)

data GoldenParams = MkGoldenParams
  { mConfig :: !(Maybe Config),
    mDisplayConfig :: !(Maybe DisplayConfig),
    testDesc :: !TestName,
    testName :: !OsPath,
    testPath :: !OsPath,
    runner :: !(Config -> DisplayConfig -> OsPath -> IO Text)
  }
