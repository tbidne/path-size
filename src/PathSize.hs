{-# LANGUAGE OverloadedLists #-}

-- | @since 0.1
module PathSize
  ( -- * Types
    PathData (..),
    SubPathData (MkSubPathData),

    -- ** Configuration
    Config (..),
    Strategy (..),

    -- * High level functions
    findLargestPaths,
    PathSizeData.display,
  )
where

import Control.Exception (Exception (displayException))
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (Foldable (foldl'))
import Data.Functor ((<&>))
import Data.HashSet qualified as HSet
import Data.Sequence (Seq (Empty, (:<|)), (<|))
import Data.Sequence qualified as Seq
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)
import Optics.Core ((^.))
import PathSize.Data
  ( Config (..),
    PathData (..),
    PathTree (..),
    Strategy (..),
    SubPathData (MkSubPathData),
  )
import PathSize.Data qualified as PathSizeData
import PathSize.Exception (PathE (MkPathE), withCallStack)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.FilePath qualified as FP
import System.Posix.Files qualified as Posix
import UnliftIO.Async qualified as Async
import UnliftIO.Exception (catchAny)

-- | Given a path, finds the size of all subpaths, recursively.
--
-- @since 0.1
findLargestPaths ::
  HasCallStack =>
  -- | Configuration.
  Config ->
  -- | Path to search.
  FilePath ->
  -- | The results. The left element are any errors encountered, while the
  -- right element is the path size data.
  IO (Seq PathE, SubPathData)
findLargestPaths cfg path = f cfg path <&> second takeLargestN
  where
    f = case cfg ^. #strategy of
      Sync -> pathDataRecursiveSync
      Async -> pathDataRecursiveAsync
      AsyncPooled -> pathDataRecursiveAsyncPooled
    takeLargestN =
      maybe
        PathSizeData.mkSubPathData
        PathSizeData.takeLargestN
        (cfg ^. #numPaths)

-- | Given a path, associates all subpaths to their size, recursively.
-- The searching is performed sequentially.
--
-- @since 0.1
pathDataRecursiveSync ::
  HasCallStack =>
  Config ->
  FilePath ->
  IO (Seq PathE, PathTree)
pathDataRecursiveSync = pathDataRecursive traverse

-- | Like 'pathDataRecursive', but each recursive call is run in its own
-- thread.
--
-- @since 0.1
pathDataRecursiveAsync ::
  HasCallStack =>
  Config ->
  FilePath ->
  IO (Seq PathE, PathTree)
pathDataRecursiveAsync = pathDataRecursive Async.mapConcurrently

-- | Like 'pathDataRecursiveAsync', but runs with a thread pool.
--
-- @since 0.1
pathDataRecursiveAsyncPooled ::
  HasCallStack =>
  Config ->
  FilePath ->
  IO (Seq PathE, PathTree)
pathDataRecursiveAsyncPooled = pathDataRecursive Async.pooledMapConcurrently

-- | Given a path, associates all subpaths to their size, recursively.
-- The searching is performed via the parameter traversal.
--
-- @since 0.1
pathDataRecursive ::
  HasCallStack =>
  -- | Traversal function.
  (forall a b t. Traversable t => (a -> IO b) -> t a -> IO (t b)) ->
  -- | The config.
  Config ->
  -- | Start path.
  FilePath ->
  IO (Seq PathE, PathTree)
pathDataRecursive traverseFn cfg =
  if cfg ^. #searchAll
    then goHidden
    else goSkipHidden
  where
    excluded = cfg ^. #exclude
    skipExcluded p = HSet.member p excluded

    -- NOTE: If filesOnly is on, then we do not calculate sizes for the
    -- directories themselves.
    dirSizeFn
      | cfg ^. #filesOnly = \_ _ -> 0
      | otherwise = (+)

    -- NOTE: If a maxDepth is given, we do not include paths that exceed
    -- the depth. Note that they are still included in size calculation for
    -- parent directories.
    depthExceeded = case cfg ^. #maxDepth of
      Nothing -> const False
      Just d -> (>= d)

    goSkipHidden :: HasCallStack => FilePath -> IO (Seq PathE, PathTree)
    goSkipHidden = go hidden 0

    goHidden :: HasCallStack => FilePath -> IO (Seq PathE, PathTree)
    goHidden = go (const False) 0

    -- Base recursive function. If the path is determined to be a symlink or
    -- file, calculates the size. If it is a directory, we recursively call
    -- go on all subpaths.
    go ::
      HasCallStack =>
      (FilePath -> Bool) ->
      Natural ->
      FilePath ->
      IO (Seq PathE, PathTree)
    go skipHidden !depth path =
      -- Determine if we should skip.
      if ((\p -> skipHidden p || skipExcluded p) . FP.takeFileName) path
        then pure ([], Nil)
        else
          calcTree `catchAny` \e -> do
            -- Save exceptions
            pure ([MkPathE path (displayException e)], Nil)
      where
        -- Perform actual calculation.
        calcTree :: HasCallStack => IO (Seq PathE, PathTree)
        calcTree = do
          -- 1. Symlinks
          --
          -- NOTE: Need to handle symlinks separately so that we:
          --   a. Do not chase.
          --   b. Ensure we call the right size function (Dir.getFileSize
          --      errors on dangling symlinks since it operates on the target).
          isSymLink <- withCallStack $ Dir.pathIsSymbolicLink path
          if isSymLink
            then withCallStack $ ([],) <$> calcSymLink path
            else do
              -- 2. Directories
              isDir <- withCallStack $ Dir.doesDirectoryExist path
              if isDir
                then do
                  files <- withCallStack $ Dir.listDirectory path
                  subTreesErrs <-
                    traverseFn
                      (go skipHidden (depth + 1) . (path </>))
                      (Seq.fromList files)
                  let (errs, subTrees) = flattenSeq subTreesErrs
                  -- Add the cost of the directory itself.
                  dirSize <- withCallStack $ Dir.getFileSize path
                  let (!subSize, !numFiles, !subDirs) = sumTrees subTrees
                      !numDirectories = subDirs + 1
                      !size = dirSizeFn (fromIntegral dirSize) subSize
                      -- Do not report subpaths if the depth is exceeded.
                      subTrees'
                        | depthExceeded depth = []
                        | otherwise = subTrees
                  pure
                    ( errs,
                      Node
                        MkPathData
                          { path,
                            size,
                            numFiles,
                            numDirectories
                          }
                        subTrees'
                    )
                else -- 3. Files
                  withCallStack $ ([],) <$> calcFile path

    sumTrees :: Seq PathTree -> (Natural, Natural, Natural)
    sumTrees = foldl' (\acc t -> acc `addTuple` getSum t) (0, 0, 0)

    getSum :: PathTree -> (Natural, Natural, Natural)
    getSum (Node (MkPathData {size, numFiles, numDirectories}) _) =
      (size, numFiles, numDirectories)
    getSum Nil = (0, 0, 0)

    addTuple (!a, !b, !c) (!a', !b', !c') = (a + a', b + b', c + c')

    -- NOTE: Detects hidden paths via a rather crude 'dot' check, with an
    -- exception for the current directory ./.
    hidden ('.' : '/' : _) = False
    hidden ('.' : _) = True
    hidden _ = False

calcSymLink :: FilePath -> IO PathTree
calcSymLink = calcSizeFn getSymLinkSize
  where
    getSymLinkSize =
      fmap Posix.fileSize . Posix.getSymbolicLinkStatus

calcFile :: FilePath -> IO PathTree
calcFile = calcSizeFn Dir.getFileSize

calcSizeFn :: Integral a => (FilePath -> IO a) -> FilePath -> IO PathTree
calcSizeFn sizeFn path =
  sizeFn path <&> \size ->
    Node
      MkPathData
        { path = path,
          size = fromIntegral size,
          numFiles = 1,
          numDirectories = 0
        }
      []

flattenSeq :: Seq (Seq a, b) -> (Seq a, Seq b)
flattenSeq Empty = (Empty, Empty)
flattenSeq ((xs, y) :<| zs) = (xs <> xs', y <| ys)
  where
    (xs', ys) = flattenSeq zs
