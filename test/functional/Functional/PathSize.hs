{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Functional.PathSize
  ( tests,
  )
where

import Control.Exception (Exception (displayException))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (Foldable (toList))
import Data.HashSet qualified as HSet
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Effects.Concurrent.Async (MonadAsync)
import Effects.Concurrent.Thread (MonadThread)
import Effects.Exception (MonadCatch, MonadThrow, throwM)
import Effects.FileSystem.Path ((</>))
import Effects.FileSystem.PathReader (MonadPathReader (..))
import Effects.IORef (MonadIORef)
import Effects.System.PosixCompat (MonadPosix)
import GHC.Num.Natural (Natural)
import PathSize
  ( PathData (MkPathData),
    PathE (MkPathE),
    PathSizeResult (..),
    Strategy (..),
  )
import PathSize qualified
import PathSize.Data.Config (Config (..))
import PathSize.Data.SubPathData.Internal
  ( SubPathData (UnsafeSubPathData),
    unSubPathData,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))

tests :: TestTree
tests =
  testGroup
    "FsSize.PathSize"
    [ calculatesSizes,
      calculatesAll,
      calculatesExcluded,
      calculatesFilesOnly,
      calculatesDepthN 0 expectedD0,
      calculatesDepthN 1 expectedD1,
      calculatesDepthN 2 expectedD2,
      displayTests,
      exceptionTests
    ]
  where
    expectedD0 =
      toSubPathData
        [ MkPathData ("test" </> "functional" </> "data" </> "success") 24601 4 6
        ]
    expectedD1 =
      toSubPathData
        [ MkPathData ("test" </> "functional" </> "data" </> "success") 24601 4 6,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2") 16400 2 4,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d1") 4105 2 1
        ]
    expectedD2 =
      toSubPathData
        [ MkPathData ("test" </> "functional" </> "data" </> "success") 24601 4 6,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2") 16400 2 4,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d2") 8206 1 2,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d1") 4105 2 1,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d1") 4098 1 1,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d1" </> "f2") 5 1 0,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d1" </> "f1") 4 1 0
        ]

calculatesSizes :: TestTree
calculatesSizes = testCase "Calculates sizes correctly" $ do
  PathSizeSuccess result <- runTest baseConfig successTestDir
  assertSubPathData expected result
  where
    expected =
      toSubPathData
        [ MkPathData ("test" </> "functional" </> "data" </> "success") 24601 4 6,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2") 16400 2 4,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d2") 8206 1 2,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d2" </> "d1") 4110 1 1,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d1") 4105 2 1,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d1") 4098 1 1,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d2" </> "d1" </> "f1") 14 1 0,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d1" </> "f2") 5 1 0,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d1" </> "f1") 4 1 0,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d1" </> "f1") 2 1 0
        ]

calculatesAll :: TestTree
calculatesAll = testCase "Includes hidden files" $ do
  PathSizeSuccess result <- runTest cfg successTestDir
  assertSubPathData expected result
  where
    cfg = baseConfig {searchAll = True}
    expected =
      toSubPathData
        [ MkPathData ("test" </> "functional" </> "data" </> "success") 28726 6 7,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2") 16400 2 4,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d2") 8206 1 2,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> ".hidden") 4113 1 1,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d2" </> "d1") 4110 1 1,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d1") 4105 2 1,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d1") 4098 1 1,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> ".hidden" </> "f1") 17 1 0,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d2" </> "d1" </> "f1") 14 1 0,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> ".h1") 12 1 0,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d1" </> "f2") 5 1 0,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d1" </> "f1") 4 1 0,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d1" </> "f1") 2 1 0
        ]

calculatesExcluded :: TestTree
calculatesExcluded = testCase "Excludes paths" $ do
  PathSizeSuccess result <- runTest cfg successTestDir
  assertSubPathData expected result
  where
    cfg = baseConfig {exclude = HSet.fromList ["d2", "f2"]}
    expected =
      toSubPathData
        [ MkPathData ("test" </> "functional" </> "data" </> "success") 8196 1 2,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d1") 4100 1 1,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d1" </> "f1") 4 1 0
        ]

calculatesFilesOnly :: TestTree
calculatesFilesOnly = testCase "Includes only files" $ do
  PathSizeSuccess result <- runTest cfg successTestDir
  assertSubPathData expected result
  where
    cfg = baseConfig {filesOnly = True}
    expected =
      toSubPathData
        [ MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d2" </> "d1" </> "f1") 14 1 0,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d1" </> "f2") 5 1 0,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d1" </> "f1") 4 1 0,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d1" </> "f1") 2 1 0,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d2" </> "d1") 0 1 1,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d2") 0 1 2,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d1") 0 1 1,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d2") 0 2 4,
          MkPathData ("test" </> "functional" </> "data" </> "success" </> "d1") 0 2 1,
          MkPathData ("test" </> "functional" </> "data" </> "success") 0 4 6
        ]

calculatesDepthN :: Natural -> SubPathData -> TestTree
calculatesDepthN n expected = testCase ("Calculates depth = " <> show n) $ do
  PathSizeSuccess result <- runTest cfg successTestDir
  assertSubPathData expected result
  where
    cfg = baseConfig {maxDepth = Just n}

displayTests :: TestTree
displayTests =
  testGroup
    "Display"
    [ displays,
      displaysReverse
    ]

displays :: TestTree
displays = testCase "Displays correctly" $ do
  PathSizeSuccess result <- runTest baseConfig successTestDir
  expected @=? PathSize.display False result
  where
    expected = T.unlines displayResults

displaysReverse :: TestTree
displaysReverse = testCase "Displays reverse correctly" $ do
  PathSizeSuccess result <- runTest baseConfig successTestDir
  expected @=? PathSize.display True result
  where
    expected = T.unlines $ reverse displayResults

displayResults :: [Text]
displayResults =
  [ T.pack ("test" </> "functional" </> "data" </> "success") <> ": 24.60K, Directories: 6, Files: 4",
    T.pack ("test" </> "functional" </> "data" </> "success" </> "d2") <> ": 16.40K, Directories: 4, Files: 2",
    T.pack ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d2") <> ": 8.21K, Directories: 2, Files: 1",
    T.pack ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d2" </> "d1") <> ": 4.11K, Directories: 1, Files: 1",
    T.pack ("test" </> "functional" </> "data" </> "success" </> "d1") <> ": 4.10K, Directories: 1, Files: 2",
    T.pack ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d1") <> ": 4.10K, Directories: 1, Files: 1",
    T.pack ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d2" </> "d1" </> "f1") <> ": 14.00B, Directories: 0, Files: 1",
    T.pack ("test" </> "functional" </> "data" </> "success" </> "d1" </> "f2") <> ": 5.00B, Directories: 0, Files: 1",
    T.pack ("test" </> "functional" </> "data" </> "success" </> "d1" </> "f1") <> ": 4.00B, Directories: 0, Files: 1",
    T.pack ("test" </> "functional" </> "data" </> "success" </> "d2" </> "d1" </> "f1") <> ": 2.00B, Directories: 0, Files: 1"
  ]

exceptionTests :: TestTree
exceptionTests =
  testGroup
    "Exceptions"
    [ testsPartial,
      testsFailure
    ]

testsPartial :: TestTree
testsPartial = testCase "Partial success" $ do
  PathSizePartial errs result <- runTest baseConfig partialTestDir
  assertSubPathData expectedResults result
  assertErrs expectedErrs errs
  where
    expectedResults =
      toSubPathData
        [ MkPathData ("test" </> "functional" </> "data" </> "partial") 8199 2 2,
          MkPathData ("test" </> "functional" </> "data" </> "partial" </> "d1") 4099 1 1,
          MkPathData ("test" </> "functional" </> "data" </> "partial" </> "f1") 4 1 0,
          MkPathData ("test" </> "functional" </> "data" </> "partial" </> "d1" </> "good") 3 1 0
        ]
    expectedErrs =
      [ MkPathE ("test" </> "functional" </> "data" </> "partial" </> "d1" </> "is-dir-err") "dir err",
        MkPathE ("test" </> "functional" </> "data" </> "partial" </> "d1" </> "is-sym-link-err") "sym link err",
        MkPathE ("test" </> "functional" </> "data" </> "partial" </> "size-err") "bad size"
      ]

testsFailure :: TestTree
testsFailure = testCase "Failure" $ do
  PathSizeFailure errs <- runTest baseConfig failureTestDir
  assertErrs expectedErrs errs
  where
    expectedErrs =
      [MkPathE ("test" </> "functional" </> "data" </> "failure") "does not exist"]

newtype FuncIO a = MkFuncIO (IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadAsync,
      MonadCatch,
      MonadIO,
      MonadIORef,
      MonadPosix,
      MonadThread,
      MonadThrow
    )
    via IO

newtype E = MkE String
  deriving stock (Show)

instance Exception E where
  displayException (MkE s) = s

instance MonadPathReader FuncIO where
  listDirectory = liftIO . listDirectory

  doesDirectoryExist p
    | p == "test" </> "functional" </> "data" </> "partial" </> "d1" </> "is-dir-err" = throwM $ MkE "dir err"
  doesDirectoryExist p = liftIO $ doesDirectoryExist p

  pathIsSymbolicLink p
    | p == "test" </> "functional" </> "data" </> "failure" = throwM $ MkE "does not exist"
    | p == "test" </> "functional" </> "data" </> "partial" </> "d1" </> "is-sym-link-err" = throwM $ MkE "sym link err"
  pathIsSymbolicLink p = liftIO $ pathIsSymbolicLink p

  getFileSize p
    | p == "test" </> "functional" </> "data" </> "partial" = pure 4096
    | p == "test" </> "functional" </> "data" </> "partial" </> "d1" = pure 4096
    | p == "test" </> "functional" </> "data" </> "partial" </> "d1" </> "good" = pure 3
    | p == "test" </> "functional" </> "data" </> "partial" </> "f1" = pure 4
    | p == "test" </> "functional" </> "data" </> "partial" </> "size-err" = throwM $ MkE "bad size"
    | p == "test" </> "functional" </> "data" </> "success" = pure 4096
    | p == "test" </> "functional" </> "data" </> "success" </> ".hidden" = pure 4096
    | p == "test" </> "functional" </> "data" </> "success" </> ".hidden" </> "f1" = pure 17
    | p == "test" </> "functional" </> "data" </> "success" </> ".h1" = pure 12
    | p == "test" </> "functional" </> "data" </> "success" </> "d1" = pure 4096
    | p == "test" </> "functional" </> "data" </> "success" </> "d1" </> "f1" = pure 4
    | p == "test" </> "functional" </> "data" </> "success" </> "d1" </> "f2" = pure 5
    | p == "test" </> "functional" </> "data" </> "success" </> "d2" = pure 4096
    | p == "test" </> "functional" </> "data" </> "success" </> "d2" </> "d1" = pure 4096
    | p == "test" </> "functional" </> "data" </> "success" </> "d2" </> "d1" </> "f1" = pure 2
    | p == "test" </> "functional" </> "data" </> "success" </> "d2" </> "d2" = pure 4096
    | p == "test" </> "functional" </> "data" </> "success" </> "d2" </> "d2" </> "d1" = pure 4096
    | p == "test" </> "functional" </> "data" </> "success" </> "d2" </> "d2" </> "d1" </> "f1" = pure 14
  getFileSize p = error p

runFuncIO :: FuncIO a -> IO a
runFuncIO (MkFuncIO io) = io

runTest :: Config -> FilePath -> IO (PathSizeResult SubPathData)
runTest cfg testDir = runFuncIO (PathSize.findLargestPaths cfg testDir)

assertSubPathData :: SubPathData -> SubPathData -> IO ()
assertSubPathData expected results =
  assertLists (sbdToList expected) (sbdToList results)
  where
    sbdToList = toList . unSubPathData

assertErrs :: [PathE] -> NESeq PathE -> IO ()
assertErrs expected results = assertLists expected (toList' results)
  where
    toList' = Set.toList . Set.fromList . toList

assertLists :: (Eq a, Show a) => [a] -> [a] -> IO ()
assertLists [] [] = pure ()
assertLists [] ys@(_ : _) = assertFailure $ "Empty expected but non-empty results: " <> show ys
assertLists xs@(_ : _) [] = assertFailure $ "Empty results but non-empty expected: " <> show xs
assertLists (x : xs) (y : ys) = (x @=? y) *> assertLists xs ys

toSubPathData :: [PathData Natural] -> SubPathData
toSubPathData [] = error "Functional.PathSize.toSubPathData: empty list!"
toSubPathData (x : xs) = UnsafeSubPathData $ x :<|| Seq.fromList xs

successTestDir :: FilePath
successTestDir = "test" </> "functional" </> "data" </> "success"

partialTestDir :: FilePath
partialTestDir = "test" </> "functional" </> "data" </> "partial"

failureTestDir :: FilePath
failureTestDir = "test" </> "functional" </> "data" </> "failure"

baseConfig :: Config
baseConfig =
  MkConfig
    { searchAll = False,
      maxDepth = Nothing,
      exclude = HSet.empty,
      filesOnly = False,
      numPaths = Nothing,
      strategy = Sync
    }
