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
      Sync -> pathDataRecursiveSync
      Async -> pathDataRecursiveAsync
      AsyncPooled -> pathDataRecursiveAsyncPooled
    takeLargestN =
      maybe
        PathSizeData.mkSubPathSizeData
        PathSizeData.takeLargestN
        (cfg ^. #numPaths)

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
pathDataRecursiveSync :: HasCallStack => FilePath -> IO PathTree
pathDataRecursiveSync = pathDataRecursive traverse

-- | Like 'pathDataRecursive', but each recursive call is run in its own
-- thread.
--
-- @since 0.1
pathDataRecursiveAsync :: HasCallStack => FilePath -> IO PathTree
pathDataRecursiveAsync = pathDataRecursive Async.mapConcurrently

-- | Like 'pathDataRecursiveAsync', but runs with a thread pool.
--
-- @since 0.1
pathDataRecursiveAsyncPooled :: HasCallStack => FilePath -> IO PathTree
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
  IO PathTree
pathDataRecursive traverseT = go
  where
    go :: HasCallStack => FilePath -> IO PathTree
    go path = do
      isFile <- withCallStack $ Dir.doesFileExist path
      if isFile
        then do
          -- NOTE: The fromIntegral :: Integer -> Natural comes w/ a slight
          -- performance penalty.
          size <- withCallStack $ fromIntegral <$> Dir.getFileSize path
          pure $ Leaf $ MkPathSizeData (File path, size)
        else do
          isDir <- withCallStack $ Dir.doesDirectoryExist path
          if isDir
            then do
              files <- withCallStack $ Dir.listDirectory path
              subTrees <- traverseT (go . (path </>)) (Seq.fromList files)
              let size = sumTrees subTrees
              pure $ Node (MkPathSizeData (Directory path, size)) subTrees
            else do
              -- NOTE: Assuming this is a symbolic link. Maybe we should warn?
              pure $ Leaf $ MkPathSizeData (File path, 0)

    sumTrees :: Seq PathTree -> Natural
    sumTrees = foldl' (\acc t -> acc + getSum t) 0

    getSum (Leaf x) = x ^. (#unPathSizeData % _2)
    getSum (Node x _) = x ^. (#unPathSizeData % _2)
