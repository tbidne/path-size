{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Exception (displayException)
import FileSystem.OsPath (decodeThrowM, encodeValidThrowM, osp)
import GHC.Conc (setUncaughtExceptionHandler)
import PathSize.Bench.Common qualified as Common
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)

  getArgs >>= \case
    ["setup"] -> do
      testPath <- Common.setup [osp|profile|]
      pathStr <- decodeThrowM testPath

      let output = "test-dir: |" ++ pathStr ++ "|"
      putStrLn output
    ["teardown", pathStr] -> do
      path <- encodeValidThrowM pathStr
      Common.teardown path
    xs -> die $ "Unexpected args: " ++ show xs
