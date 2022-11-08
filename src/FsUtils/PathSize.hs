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
import Optics.Core ((%), (^.), _2)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.FilePath qualified as FP
import UnliftIO.Async qualified as Async

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
findLargestPaths cfg path = f path <&> \pathTree -> takeLargestN pathTree
  where
    f = case cfg ^. #strategy of
      Sync -> pathDataRecursiveSync (cfg ^. #searchAll)
      Async -> pathDataRecursiveAsync (cfg ^. #searchAll)
      AsyncPooled -> pathDataRecursiveAsyncPooled (cfg ^. #searchAll)
    takeLargestN =
      maybe
        PathSizeData.mkSubPathSizeData
        PathSizeData.takeLargestN
        (cfg ^. #numPaths)

-- | Given a path, associates all subpaths to their size, recursively.
-- The searching is performed sequentially.
--
-- @since 0.1
pathDataRecursiveSync :: HasCallStack => Bool -> FilePath -> IO PathTree
pathDataRecursiveSync = pathDataRecursive traverse

-- | Like 'pathDataRecursive', but each recursive call is run in its own
-- thread.
--
-- @since 0.1
pathDataRecursiveAsync :: HasCallStack => Bool -> FilePath -> IO PathTree
pathDataRecursiveAsync = pathDataRecursive Async.mapConcurrently

-- | Like 'pathDataRecursiveAsync', but runs with a thread pool.
--
-- @since 0.1
pathDataRecursiveAsyncPooled :: HasCallStack => Bool -> FilePath -> IO PathTree
pathDataRecursiveAsyncPooled = pathDataRecursive Async.pooledMapConcurrently

-- | Given a path, associates all subpaths to their size, recursively.
-- The searching is performed via the parameter traversal.
--
-- @since 0.1
pathDataRecursive ::
  HasCallStack =>
  -- | Traversal function.
  (forall a b t. Traversable t => (a -> IO b) -> t a -> IO (t b)) ->
  -- | If true, searches hidden files/directories.
  Bool ->
  -- | Start path.
  FilePath ->
  IO PathTree
pathDataRecursive traverseFn = \case
  True -> goHidden
  False -> goSkipHidden
  where
    goSkipHidden :: HasCallStack => FilePath -> IO PathTree
    goSkipHidden = go hidden

    goHidden :: HasCallStack => FilePath -> IO PathTree
    goHidden = go (const False)

    go :: HasCallStack => (FilePath -> Bool) -> FilePath -> IO PathTree
    go skipPath path =
      if skipPath path
        then pure Nil
        else do
          isFile <- withCallStack $ Dir.doesFileExist path
          if isFile
            then do
              -- NOTE: The fromIntegral :: Integer -> Natural comes w/ a slight
              -- performance penalty.
              size <- withCallStack $ fromIntegral <$> Dir.getFileSize path
              pure $ Node (MkPathSizeData (File path, size)) []
            else do
              isDir <- withCallStack $ Dir.doesDirectoryExist path
              if isDir
                then do
                  files <- withCallStack $ Dir.listDirectory path
                  subTrees <-
                    traverseFn
                      (go skipPath . (path </>))
                      (Seq.fromList files)
                  let size = sumTrees subTrees
                  pure $ Node (MkPathSizeData (Directory path, size)) subTrees
                else do
                  -- NOTE: Assuming this is a symbolic link. Maybe we should
                  -- warn? Or add the size of the link itself?
                  pure $ Node (MkPathSizeData (File path, 0)) []

    sumTrees :: Seq PathTree -> Natural
    sumTrees = foldl' (\acc t -> acc + getSum t) 0

    getSum (Node x _) = x ^. (#unPathSizeData % _2)
    getSum Nil = 0

    hidden = hidden' . FP.takeFileName

    -- NOTE: Detects hidden paths via a rather crude 'dot' check, with an
    -- exception for the current directory ./.
    hidden' ('.' : '/' : _) = False
    hidden' ('.' : _) = True
    hidden' _ = False
