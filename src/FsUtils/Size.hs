{-# LANGUAGE OverloadedLists #-}

-- | @since 0.1
module FsUtils.Size
  ( -- * Types
    Path (..),
    SizedPath (..),

    -- * Calculating sizes
    pathSizeRecursive,
    pathSizeRecursiveAsync,
    pathSizeRecursiveParallel,
  )
where

import Control.Monad (join)
import Data.Foldable (Foldable (foldl'))
import Data.Sequence (Seq, (<|))
import FsUtils.Control.Exception (withCallStack)
import FsUtils.Data.PathSize (Path (..), SizedPath (..), sumPathSize)
import GHC.Stack (HasCallStack)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import UnliftIO.Async qualified as Async

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
pathSizeRecursive :: HasCallStack => FilePath -> IO (Seq SizedPath)
pathSizeRecursive = pathSizeRecursiveTraversal traverse

-- | Like 'pathSizeRecursive', but each recursive call is run in its own
-- thread.
--
-- @since 0.1
pathSizeRecursiveAsync :: HasCallStack => FilePath -> IO (Seq SizedPath)
pathSizeRecursiveAsync = pathSizeRecursiveTraversal Async.mapConcurrently

-- | Like 'pathSizeRecursive', but each recursive call is run in parallel.
--
-- @since 0.1
pathSizeRecursiveParallel :: HasCallStack => FilePath -> IO (Seq SizedPath)
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
  IO (Seq SizedPath)
pathSizeRecursiveTraversal traverseT = go []
  where
    go :: HasCallStack => Seq SizedPath -> FilePath -> IO (Seq SizedPath)
    go mp path = do
      isFile <- withCallStack $ Dir.doesFileExist path
      if isFile
        then do
          size <- withCallStack $ Dir.getFileSize path
          pure $ MkSizedPath (File path, size) <| mp
        else do
          isDir <- withCallStack $ Dir.doesDirectoryExist path
          if isDir
            then do
              files <- withCallStack $ Dir.listDirectory path
              -- NOTE: Benchmarking shows it is significantly better to do
              -- the "list to seq + join" here as opposed to after.
              maps <- join <$> traverseT (go mp)
                (foldl' (\acc f -> (path </> f) <| acc) [] files)
              let size = sumPathSize maps
              pure $ MkSizedPath (Directory path, size) <| (mp <> maps)
            else do
              -- NOTE: Assuming this is a symbolic link. Maybe we should warn?
              pure mp
