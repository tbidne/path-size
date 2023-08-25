{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Functional.PathSize
  ( tests,
  )
where

import Control.Exception (Exception (displayException))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (Foldable (toList))
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as HSet
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)
import Effects.Concurrent.Async (MonadAsync)
import Effects.Concurrent.Thread (MonadThread)
import Effects.Exception (MonadCatch, MonadThrow, throwM)
import Effects.FileSystem.PathReader (MonadPathReader)
import Effects.FileSystem.PathReader qualified as RDir
import Effects.FileSystem.Utils qualified as FsUtils
import Effects.IORef (MonadIORef)
import Effects.System.PosixCompat (MonadPosixCompat)
import GHC.Num.Natural (Natural)
import PathSize
  ( PathData (MkPathData),
    PathE (MkPathE),
    PathSizeResult (PathSizeFailure, PathSizePartial, PathSizeSuccess),
    Strategy (Sync),
  )
import PathSize qualified
import PathSize.Data.Config
  ( Config
      ( MkConfig,
        exclude,
        filesOnly,
        maxDepth,
        numPaths,
        searchAll,
        stableSort,
        strategy
      ),
  )
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
        [ mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success") 24601 4 6
        ]
    expectedD1 =
      toSubPathData
        [ mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success") 24601 4 6,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2") 16400 2 4,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1") 4105 2 1
        ]
    expectedD2 =
      toSubPathData
        [ mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success") 24601 4 6,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2") 16400 2 4,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d2") 8206 1 2,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1") 4105 2 1,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d1") 4098 1 1,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1" `cfp` "f2") 5 1 0,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1" `cfp` "f1") 4 1 0
        ]

mkPathData :: String -> a -> a -> a -> PathData a
mkPathData p size numFiles numDirectories =
  case FsUtils.encodeFpToOs p of
    Left ex -> error $ "Error creating OsPath in func test: " ++ displayException ex
    Right path ->
      MkPathData
        { path,
          size,
          numFiles,
          numDirectories
        }

mkPathE :: String -> String -> PathE
mkPathE p err =
  case FsUtils.encodeFpToOs p of
    Left ex -> error $ "Error creating PathE in func test: " ++ displayException ex
    Right path -> MkPathE path err

calculatesSizes :: TestTree
calculatesSizes = testCase "Calculates sizes correctly" $ do
  PathSizeSuccess result <- runTest baseConfig successTestDir
  assertSubPathData expected result
  where
    expected =
      toSubPathData
        [ mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success") 24601 4 6,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2") 16400 2 4,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d2") 8206 1 2,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d2" `cfp` "d1") 4110 1 1,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1") 4105 2 1,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d1") 4098 1 1,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d2" `cfp` "d1" `cfp` "f1") 14 1 0,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1" `cfp` "f2") 5 1 0,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1" `cfp` "f1") 4 1 0,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d1" `cfp` "f1") 2 1 0
        ]

calculatesAll :: TestTree
calculatesAll = testCase "Includes hidden files" $ do
  PathSizeSuccess result <- runTest cfg successTestDir
  assertSubPathData expected result
  where
    cfg = baseConfig {searchAll = True}
    expected =
      toSubPathData
        [ mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success") 28726 6 7,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2") 16400 2 4,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d2") 8206 1 2,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` ".hidden") 4113 1 1,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d2" `cfp` "d1") 4110 1 1,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1") 4105 2 1,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d1") 4098 1 1,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` ".hidden" `cfp` "f1") 17 1 0,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d2" `cfp` "d1" `cfp` "f1") 14 1 0,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` ".h1") 12 1 0,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1" `cfp` "f2") 5 1 0,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1" `cfp` "f1") 4 1 0,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d1" `cfp` "f1") 2 1 0
        ]

calculatesExcluded :: TestTree
calculatesExcluded = testCase "Excludes paths" $ do
  excluded <- traverse FsUtils.encodeFpToOsThrowM ["d2", "f2"]
  let cfg = baseConfig {exclude = HSet.fromList excluded}
  PathSizeSuccess result <- runTest cfg successTestDir
  assertSubPathData expected result
  where
    expected =
      toSubPathData
        [ mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success") 8196 1 2,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1") 4100 1 1,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1" `cfp` "f1") 4 1 0
        ]

calculatesFilesOnly :: TestTree
calculatesFilesOnly = testCase "Includes only files" $ do
  PathSizeSuccess result <- runTest cfg successTestDir
  assertSubPathData expected result
  where
    cfg = baseConfig {filesOnly = True}
    expected =
      toSubPathData
        [ mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d2" `cfp` "d1" `cfp` "f1") 14 1 0,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1" `cfp` "f2") 5 1 0,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1" `cfp` "f1") 4 1 0,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d1" `cfp` "f1") 2 1 0,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d2" `cfp` "d1") 0 1 1,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d2") 0 1 2,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d1") 0 1 1,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2") 0 2 4,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1") 0 2 1,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "success") 0 4 6
        ]

calculatesDepthN :: Word16 -> SubPathData -> TestTree
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
  [ T.pack ("test" `cfp` "functional" `cfp` "data" `cfp` "success") <> ": 24.60K, Directories: 6, Files: 4",
    T.pack ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2") <> ": 16.40K, Directories: 4, Files: 2",
    T.pack ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d2") <> ": 8.21K, Directories: 2, Files: 1",
    T.pack ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d2" `cfp` "d1") <> ": 4.11K, Directories: 1, Files: 1",
    T.pack ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1") <> ": 4.10K, Directories: 1, Files: 2",
    T.pack ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d1") <> ": 4.10K, Directories: 1, Files: 1",
    T.pack ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d2" `cfp` "d1" `cfp` "f1") <> ": 14.00B, Directories: 0, Files: 1",
    T.pack ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1" `cfp` "f2") <> ": 5.00B, Directories: 0, Files: 1",
    T.pack ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1" `cfp` "f1") <> ": 4.00B, Directories: 0, Files: 1",
    T.pack ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d1" `cfp` "f1") <> ": 2.00B, Directories: 0, Files: 1"
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
        [ mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "partial") 8199 2 2,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "partial" `cfp` "d1") 4099 1 1,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "partial" `cfp` "f1") 4 1 0,
          mkPathData ("test" `cfp` "functional" `cfp` "data" `cfp` "partial" `cfp` "d1" `cfp` "good") 3 1 0
        ]
    expectedErrs =
      [ mkPathE ("test" `cfp` "functional" `cfp` "data" `cfp` "partial" `cfp` "d1" `cfp` "is-dir-err") "dir err",
        mkPathE ("test" `cfp` "functional" `cfp` "data" `cfp` "partial" `cfp` "d1" `cfp` "is-sym-link-err") "sym link err",
        mkPathE ("test" `cfp` "functional" `cfp` "data" `cfp` "partial" `cfp` "size-err") "bad size"
      ]

testsFailure :: TestTree
testsFailure = testCase "Failure" $ do
  PathSizeFailure errs <- runTest baseConfig failureTestDir
  assertErrs expectedErrs errs
  where
    expectedErrs =
      [mkPathE ("test" `cfp` "functional" `cfp` "data" `cfp` "failure") "does not exist"]

newtype FuncIO a = MkFuncIO (IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadAsync,
      MonadCatch,
      MonadIO,
      MonadIORef,
      MonadPosixCompat,
      MonadThread,
      MonadThrow
    )
    via IO

newtype E = MkE String
  deriving stock (Show)

instance Exception E where
  displayException (MkE s) = s

instance MonadPathReader FuncIO where
  listDirectory = liftIO . RDir.listDirectory

  doesDirectoryExist p = do
    path <- FsUtils.decodeOsToFpThrowM p
    if path == "test" `cfp` "functional" `cfp` "data" `cfp` "partial" `cfp` "d1" `cfp` "is-dir-err"
      then throwM $ MkE "dir err"
      else liftIO $ RDir.doesDirectoryExist p

  pathIsSymbolicLink p = do
    path <- FsUtils.decodeOsToFpThrowM p
    if
      | path == "test" `cfp` "functional" `cfp` "data" `cfp` "failure" -> throwM $ MkE "does not exist"
      | path == "test" `cfp` "functional" `cfp` "data" `cfp` "partial" `cfp` "d1" `cfp` "is-sym-link-err" -> throwM $ MkE "sym link err"
      | otherwise -> liftIO $ RDir.pathIsSymbolicLink p

  getFileSize p = do
    path <- FsUtils.decodeOsToFpThrowM p
    case Map.lookup path mp of
      Just m -> m
      Nothing -> error "p"
    where
      mp =
        Map.fromList
          [ ("test" `cfp` "functional" `cfp` "data" `cfp` "partial", pure 4096),
            ("test" `cfp` "functional" `cfp` "data" `cfp` "partial" `cfp` "d1", pure 4096),
            ("test" `cfp` "functional" `cfp` "data" `cfp` "partial" `cfp` "d1" `cfp` "good", pure 3),
            ("test" `cfp` "functional" `cfp` "data" `cfp` "partial" `cfp` "f1", pure 4),
            ("test" `cfp` "functional" `cfp` "data" `cfp` "partial" `cfp` "size-err", throwM (MkE "bad size")),
            ("test" `cfp` "functional" `cfp` "data" `cfp` "success", pure 4096),
            ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` ".hidden", pure 4096),
            ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` ".hidden" `cfp` "f1", pure 17),
            ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` ".h1", pure 12),
            ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1", pure 4096),
            ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1" `cfp` "f1", pure 4),
            ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d1" `cfp` "f2", pure 5),
            ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2", pure 4096),
            ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d1", pure 4096),
            ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d1" `cfp` "f1", pure 2),
            ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d2", pure 4096),
            ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d2" `cfp` "d1", pure 4096),
            ("test" `cfp` "functional" `cfp` "data" `cfp` "success" `cfp` "d2" `cfp` "d2" `cfp` "d1" `cfp` "f1", pure 14),
            ("test" `cfp` "functional" `cfp` "data" `cfp` "partial", pure 4096)
          ]

runFuncIO :: FuncIO a -> IO a
runFuncIO (MkFuncIO io) = io

runTest :: Config -> FilePath -> IO (PathSizeResult SubPathData)
runTest cfg testDir = do
  testDir' <- FsUtils.encodeFpToOsThrowM testDir
  runFuncIO (PathSize.findLargestPaths cfg testDir')

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
successTestDir = "test" `cfp` "functional" `cfp` "data" `cfp` "success"

partialTestDir :: FilePath
partialTestDir = "test" `cfp` "functional" `cfp` "data" `cfp` "partial"

failureTestDir :: FilePath
failureTestDir = "test" `cfp` "functional" `cfp` "data" `cfp` "failure"

baseConfig :: Config
baseConfig =
  MkConfig
    { searchAll = False,
      maxDepth = Nothing,
      exclude = HSet.empty,
      filesOnly = False,
      numPaths = Nothing,
      -- True because we want tests to be deterministic
      stableSort = True,
      strategy = Sync
    }

-- For brevity
cfp :: FilePath -> FilePath -> FilePath
cfp = FsUtils.combineFilePaths
