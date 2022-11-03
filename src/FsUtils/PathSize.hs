{-# LANGUAGE OverloadedLists #-}

-- | @since 0.1
module FsUtils.PathSize
  ( -- * Types
    Path (..),
    PathSizeData (..),

    -- ** Configuration
    PathSizeConfig (..),
    Strategy (..),

    -- * High level functions
    findLargestPaths,
    PathSizeData.display,

    -- * Calculating total size
    pathDataRecursiveSync,
    pathDataRecursiveAsync,
    pathDataRecursiveAsyncPooled,
  )
where

import Control.Monad (join)
import Data.Foldable (Foldable (foldl'))
import Data.Functor ((<&>))
import Data.Sequence (Seq, (<|))
import FsUtils.Control.Exception (withCallStack)
import FsUtils.Data.PathSizeConfig (PathSizeConfig (..), Strategy (..))
import FsUtils.Data.PathSizeData (Path (..), PathSizeData (..))
import FsUtils.Data.PathSizeData qualified as PathSizeData
import GHC.Stack (HasCallStack)
import Optics.Core ((^.))
import System.Directory qualified as Dir
import System.FilePath ((</>))
import UnliftIO.Async qualified as Async

-- | Given a path, finds the size of all subpaths, recursively.
--
-- @since 0.1
findLargestPaths ::
  -- | Configuration.
  PathSizeConfig ->
  -- | Path to search.
  FilePath ->
  IO (Seq PathSizeData)
findLargestPaths cfg path = f path <&> \dataSeq -> takeLargestN dataSeq
  where
    -- TODO: We should probably ensure the return list is always sorted.
    f = case cfg ^. #strategy of
      Sync -> pathDataRecursiveSync
      Async -> pathDataRecursiveAsync
      AsyncPooled -> pathDataRecursiveAsyncPooled
    takeLargestN = maybe id PathSizeData.takeLargestN (cfg ^. #numPaths)

-- TODO: Configuration options to consider:
--
-- 1. Depth: limit the depth for the data we hold. That is, past some d, do not
--    store any paths lower than d. Note we still have to recurse to the end
--    to get accurate sizes. We can experiment with a utility like du,
--    to see if there is any benefit to delegating that final calculation to
--    to du rather than doing it ourselves directly.
--
-- 2. Exclude: skip given directories (HashSet), files.
--
-- 3. Hidden: skip hidden directories (files too?).

-- | Given a path, associates all subpaths to their size, recursively.
-- The searching is performed sequentially.
--
-- @since 0.1
pathDataRecursiveSync :: HasCallStack => FilePath -> IO (Seq PathSizeData)
pathDataRecursiveSync = pathDataRecursive traverse

-- | Like 'pathDataRecursive', but each recursive call is run in its own
-- thread.
--
-- @since 0.1
pathDataRecursiveAsync :: HasCallStack => FilePath -> IO (Seq PathSizeData)
pathDataRecursiveAsync = pathDataRecursive Async.mapConcurrently

-- | Like 'pathDataRecursiveAsync', but runs with a thread pool.
--
-- @since 0.1
pathDataRecursiveAsyncPooled :: HasCallStack => FilePath -> IO (Seq PathSizeData)
pathDataRecursiveAsyncPooled = pathDataRecursive Async.pooledMapConcurrently

-- | Given a path, associates all subpaths to their size, recursively.
-- The searching is performed via the parameter traversal.
--
-- @since 0.1
pathDataRecursive ::
  HasCallStack =>
  -- | Traversal function.
  (forall a b t. Traversable t => (a -> IO b) -> t a -> IO (t b)) ->
  -- | Start path.
  FilePath ->
  IO (Seq PathSizeData)
pathDataRecursive traverseT = go []
  where
    go :: HasCallStack => Seq PathSizeData -> FilePath -> IO (Seq PathSizeData)
    go mp path = do
      isFile <- withCallStack $ Dir.doesFileExist path
      if isFile
        then do
          size <- withCallStack $ Dir.getFileSize path
          pure $ MkPathSizeData (File path, size) <| mp
        else do
          isDir <- withCallStack $ Dir.doesDirectoryExist path
          if isDir
            then do
              files <- withCallStack $ Dir.listDirectory path
              -- NOTE: Benchmarking shows it is significantly better to do
              -- the "list to seq + join" here as opposed to after.
              maps <-
                join
                  <$> traverseT
                    (go mp)
                    (foldl' (\acc f -> (path </> f) <| acc) [] files)
              let size = PathSizeData.sumSize maps
              pure $ MkPathSizeData (Directory path, size) <| (mp <> maps)
            else do
              -- NOTE: Assuming this is a symbolic link. Maybe we should warn?
              pure mp
