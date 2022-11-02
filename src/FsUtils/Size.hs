-- | @since 0.1
module FsUtils.Size
  ( -- * Types
    Path (..),
    PathSize,

    -- * Calculating sizes
    pathSizeRecursive,
    pathSizeRecursiveAsync,
    pathSizeRecursiveParallel,
  )
where

import Data.HashMap.Strict qualified as HMap
import FsUtils.Control.Exception (withCallStack)
import FsUtils.Data.PathSize (Path (..), PathSize, sumPathSizes)
import GHC.Stack (HasCallStack)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import UnliftIO.Async qualified as Async

-- TODO: Performance is likely not good enough (takes many minutes for my
-- home dir), maybe due to memory use of holding a gigantic map.
-- Perhaps we want to "limit the depth" i.e. take an argument that prevents
-- building our map past a certain point. We'd still need to traverse the
-- entire tree to get accurate sizes, but we'd only store elements up to
-- some point.
--
-- If this is still too slow, consider using a fast utility like du.

-- | Given a path, associates all subpaths to their size, recursively.
-- The searching is performed sequentially.
--
-- @since 0.1
pathSizeRecursive :: HasCallStack => FilePath -> IO PathSize
pathSizeRecursive = pathSizeRecursiveTraversal traverse

-- | Like 'pathSizeRecursive', but each recursive call is run in its own
-- thread.
--
-- @since 0.1
pathSizeRecursiveAsync :: HasCallStack => FilePath -> IO PathSize
pathSizeRecursiveAsync = pathSizeRecursiveTraversal Async.mapConcurrently

-- | Like 'pathSizeRecursive', but each recursive call is run in parallel.
--
-- @since 0.1
pathSizeRecursiveParallel :: HasCallStack => FilePath -> IO PathSize
pathSizeRecursiveParallel = pathSizeRecursiveTraversal Async.pooledMapConcurrently

-- | Given a path, associates all subpaths to their size, recursively.
-- The searching is performed via the parameter traversal.
--
-- @since 0.1
pathSizeRecursiveTraversal ::
  HasCallStack =>
  -- | Traversal function.
  (forall a b t. Traversable t => (a -> IO b) -> t a -> IO (t b)) ->
  -- | Start path.
  FilePath ->
  IO PathSize
pathSizeRecursiveTraversal traverseT = go HMap.empty
  where
    go :: HasCallStack => PathSize -> FilePath -> IO PathSize
    go mp path = do
      isFile <- withCallStack $ Dir.doesFileExist path
      if isFile
        then do
          size <- withCallStack $ Dir.getFileSize path
          pure $ HMap.insert (File path) size mp
        else do
          isDir <- withCallStack $ Dir.doesDirectoryExist path
          if isDir
            then do
              files <- withCallStack $ Dir.listDirectory path
              maps <- traverseT (go mp) ((path </>) <$> files)
              let size = sumPathSizes maps
              pure $ HMap.insert (Directory path) size (HMap.unions (mp : maps))
            else do
              -- NOTE: Assuming this is a symbolic link. Maybe we should warn?
              pure mp
