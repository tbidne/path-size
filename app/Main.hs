-- | Main module.
--
-- @since 0.1
module Main (main) where

import Args (argsToConfig, getArgs)
import Control.Exception (Exception (displayException))
import Data.Foldable (for_)
import Data.Text qualified as T
import Effectful (Eff, IOE, runEff)
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent qualified as CC
import Effectful.FileSystem.PathReader.Dynamic (PathReaderDynamic)
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.PosixCompat.Static (PosixCompatStatic)
import Effectful.PosixCompat.Static qualified as Posix
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import Optics.Core ((^.))
import PathSize
  ( PathSizeResult (PathSizeFailure, PathSizePartial, PathSizeSuccess),
    display,
    findLargestPaths,
  )

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)

  args <- getArgs
  let config = argsToConfig args
      printResults = putStrLn . T.unpack . display (args ^. #reverseSort)

  runPathSize (findLargestPaths config (args ^. #path)) >>= \case
    PathSizeSuccess sbd -> printResults sbd
    PathSizePartial errs sbd -> do
      for_ errs (putStrLn . displayException)
      putStrLn ""
      printResults sbd
    PathSizeFailure errs -> for_ errs (putStrLn . displayException)
  where
    runPathSize ::
      Eff
        [ PosixCompatStatic,
          Concurrent,
          PathReaderDynamic,
          IOE
        ]
        a ->
      IO a
    runPathSize =
      runEff
        . PR.runPathReaderDynamicIO
        . CC.runConcurrent
        . Posix.runPosixCompatStaticIO
