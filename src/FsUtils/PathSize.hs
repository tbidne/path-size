{-# LANGUAGE OverloadedLists #-}

-- | @since 0.1
module FsUtils.PathSize
  ( -- * Types
    Path (..),
    PathSizeData (..),
    SubPathSizeData,

    -- ** Configuration
    PathSizeConfig (..),
    Strategy (..),

    -- * High level functions
    findLargestPaths,
    PathSizeData.display,
  )
where

import Data.Foldable (Foldable (foldl'))
import Data.Functor ((<&>))
import Data.HashSet (HashSet)
import Data.HashSet qualified as HSet
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import FsUtils.Control.Exception (withCallStack)
import FsUtils.Data.PathSizeConfig (PathSizeConfig (..), Strategy (..))
import FsUtils.Data.PathSizeData
  ( Path (..),
    PathSizeData (..),
    PathTree (..),
    SubPathSizeData,
  )
import FsUtils.Data.PathSizeData qualified as PathSizeData
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)
import Optics.Core ((^.))
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.FilePath qualified as FP
import System.Posix.Files qualified as Posix
import UnliftIO.Async qualified as Async
import UnliftIO.Exception (Exception (displayException), catchAny)

-- | Given a path, finds the size of all subpaths, recursively.
--
-- @since 0.1
findLargestPaths ::
  HasCallStack =>
  -- | Configuration.
  PathSizeConfig ->
  -- | Path to search.
  FilePath ->
  IO SubPathSizeData
findLargestPaths cfg path =
  f (cfg ^. #exclude) (cfg ^. #searchAll) path
    <&> \pathTree -> takeLargestN pathTree
  where
    f = case cfg ^. #strategy of
      Sync -> pathDataRecursiveSync
      Async -> pathDataRecursiveAsync
      AsyncPooled -> pathDataRecursiveAsyncPooled
    takeLargestN =
      maybe
        PathSizeData.mkSubPathSizeData
        PathSizeData.takeLargestN
        (cfg ^. #numPaths)

-- | Given a path, associates all subpaths to their size, recursively.
-- The searching is performed sequentially.
--
-- @since 0.1
pathDataRecursiveSync ::
  HasCallStack =>
  HashSet FilePath ->
  Bool ->
  FilePath ->
  IO PathTree
pathDataRecursiveSync = pathDataRecursive traverse

-- | Like 'pathDataRecursive', but each recursive call is run in its own
-- thread.
--
-- @since 0.1
pathDataRecursiveAsync ::
  HasCallStack =>
  HashSet FilePath ->
  Bool ->
  FilePath ->
  IO PathTree
pathDataRecursiveAsync = pathDataRecursive Async.mapConcurrently

-- | Like 'pathDataRecursiveAsync', but runs with a thread pool.
--
-- @since 0.1
pathDataRecursiveAsyncPooled ::
  HasCallStack =>
  HashSet FilePath ->
  Bool ->
  FilePath ->
  IO PathTree
pathDataRecursiveAsyncPooled = pathDataRecursive Async.pooledMapConcurrently

-- | Given a path, associates all subpaths to their size, recursively.
-- The searching is performed via the parameter traversal.
--
-- @since 0.1
pathDataRecursive ::
  HasCallStack =>
  -- | Traversal function.
  (forall a b t. Traversable t => (a -> IO b) -> t a -> IO (t b)) ->
  -- | Paths to exclude.
  HashSet FilePath ->
  -- | If true, searches hidden files/directories.
  Bool ->
  -- | Start path.
  FilePath ->
  IO PathTree
pathDataRecursive traverseFn excluded = \case
  True -> goHidden
  False -> goSkipHidden
  where
    goSkipHidden :: HasCallStack => FilePath -> IO PathTree
    goSkipHidden = go hidden

    goHidden :: HasCallStack => FilePath -> IO PathTree
    goHidden = go (const False)

    go :: HasCallStack => (FilePath -> Bool) -> FilePath -> IO PathTree
    go skipHidden path =
      if ((\p -> skipHidden p || HSet.member p excluded) . FP.takeFileName) path
        then pure Nil
        else
          calcTree `catchAny` \e -> do
            putStrLn $
              mconcat
                [ "Exception with path '",
                  path,
                  "': ",
                  displayException e,
                  "\n"
                ]
            pure Nil
      where
        calcTree :: HasCallStack => IO PathTree
        calcTree = do
          -- NOTE: Do not chase symlinks, and ensure we call the right size
          -- function (Dir.getFileSize errors on dangling symlinks since it
          -- operates on the target)
          isSymLink <- withCallStack $ Dir.pathIsSymbolicLink path
          if isSymLink
            then
              withCallStack $
                getSymLinkSize path <&> \size ->
                  Node (MkPathSizeData (File path) size 1 0) []
            else do
              isDir <- withCallStack $ Dir.doesDirectoryExist path
              if isDir
                then do
                  files <- withCallStack $ Dir.listDirectory path
                  subTrees <-
                    traverseFn
                      (go skipHidden . (path </>))
                      (Seq.fromList files)
                  -- add the cost of the directory itself.
                  dirSize <- withCallStack $ Dir.getFileSize path
                  let (!subSize, !numFiles, !subDirs) = sumTrees subTrees
                      !numDirectories = subDirs + 1
                      !size = fromIntegral dirSize + subSize
                  pure $
                    Node
                      MkPathSizeData
                        { path = Directory path,
                          size,
                          numFiles,
                          numDirectories
                        }
                      subTrees
                else
                  withCallStack $
                    Dir.getFileSize path <&> \size ->
                      Node
                        MkPathSizeData
                          { path = File path,
                            size = fromIntegral size,
                            numFiles = 1,
                            numDirectories = 0
                          }
                        []

    getSymLinkSize :: FilePath -> IO Natural
    getSymLinkSize =
      fmap (fromIntegral . Posix.fileSize) . Posix.getSymbolicLinkStatus

    sumTrees :: Seq PathTree -> (Natural, Natural, Natural)
    sumTrees = foldl' (\acc t -> acc `addTuple` getSum t) (0, 0, 0)

    getSum :: PathTree -> (Natural, Natural, Natural)
    getSum (Node (MkPathSizeData {size, numFiles, numDirectories}) _) =
      (size, numFiles, numDirectories)
    getSum Nil = (0, 0, 0)

    addTuple (!a, !b, !c) (!a', !b', !c') = (a + a', b + b', c + c')

    -- NOTE: Detects hidden paths via a rather crude 'dot' check, with an
    -- exception for the current directory ./.
    hidden ('.' : '/' : _) = False
    hidden ('.' : _) = True
    hidden _ = False
