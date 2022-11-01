-- | @since 0.1
module FsUtils.Size
  ( -- * Types
    Path (..),
    PathSize,

    -- * Calculating sizes
    pathSizeRecursive,
    pathSizeRecursiveAsync,
  )
where

import Control.Concurrent.Async qualified as Async
import Control.Exception.Safe (throwString)
import Data.HashMap.Strict qualified as HMap
import FsUtils.Control.Exception (withCallStack)
import FsUtils.Data.PathSize (Path (..), PathSize, sumPathSizes)
import GHC.Stack (HasCallStack)
import System.Directory qualified as Dir
import System.FilePath ((</>))

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
pathSizeRecursiveTraversal traversal = go HMap.empty
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
              maps <- traversal (go mp) ((path </>) <$> files)
              let size = sumPathSizes maps
              pure $ HMap.insert (Directory path) size (HMap.unions (mp : maps))
            else throwString ("Not a path or directory: " <> path)
