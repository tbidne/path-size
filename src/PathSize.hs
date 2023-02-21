{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}

-- | @since 0.1
module PathSize
  ( -- * Types
    PathData (..),
    SubPathData (MkSubPathData),
    PathSizeResult (..),

    -- ** Configuration
    Config (..),
    Strategy (..),

    -- * High level functions
    findLargestPaths,
    pathSizeRecursive,
    pathSizeRecursiveConfig,
    SPD.display,

    -- * Errors
    PathE (..),
  )
where

import Data.Foldable (Foldable (foldl'))
import Data.Functor ((<&>))
import Data.HashSet qualified as HSet
import Data.Sequence (Seq (Empty, (:<|)), (<|))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Effects.Concurrent.Async qualified as Async
import Effects.Exception (catchAny, displayNoCS)
import Effects.FileSystem.Path (Path, (</>))
import Effects.FileSystem.PathReader (MonadPathReader (..))
import GHC.Natural (Natural)
import Optics.Core ((^.))
import PathSize.Data.Config (Config (..), Strategy (..))
import PathSize.Data.Config.TH (defaultNumPathsSize)
import PathSize.Data.PathData (PathData (..))
import PathSize.Data.PathSizeResult (PathSizeResult (..))
import PathSize.Data.PathTree (PathTree (..), emptyPathTree)
import PathSize.Data.SubPathData (SubPathData (MkSubPathData))
import PathSize.Data.SubPathData qualified as SPD
import PathSize.Exception (PathE (MkPathE))
#if MIN_VERSION_filepath(1,4,100)
import System.OsPath qualified as FP
#else
import System.FilePath qualified as FP
#endif
import System.Posix.Files qualified as Posix

-- | Given a path, finds the size of all subpaths, recursively.
--
-- @since 0.1
findLargestPaths ::
  -- | Configuration.
  Config ->
  -- | Path to search.
  Path ->
  -- | The results.
  IO (PathSizeResult SubPathData)
findLargestPaths cfg path = do
  f cfg path <&> \case
    -- 1. Success, received data and no errors
    (Empty, tree) -> PathSizeSuccess (takeLargestN tree)
    -- 2. Partial success, received data and some errs
    (e :<| es, tree) -> PathSizePartial (e :<|| es) (takeLargestN tree)
  where
    f = case cfg ^. #strategy of
      Sync -> pathDataRecursiveSync
      Async -> pathDataRecursiveAsync
      AsyncPooled -> pathDataRecursiveAsyncPooled
    takeLargestN =
      maybe
        SPD.mkSubPathData
        SPD.takeLargestN
        (cfg ^. #numPaths)

-- | Returns the total path size in bytes. Calls 'pathSizeRecursiveConfig' with
-- the following config:
--
-- @
-- MkConfig
--   { searchAll = True,
--     maxDepth = Just 0,
--     exclude = mempty,
--     filesOnly = False,
--     numPaths = Just 1,
--     strategy = mempty
--   }
-- @
--
-- @since 0.1
pathSizeRecursive :: Path -> IO (PathSizeResult Natural)
pathSizeRecursive = pathSizeRecursiveConfig cfg
  where
    cfg =
      MkConfig
        { searchAll = True,
          maxDepth = Just 0,
          exclude = mempty,
          filesOnly = False,
          numPaths = Just defaultNumPathsSize,
          strategy = mempty
        }

-- | Returns the total path size in bytes.
--
-- @since 0.1
pathSizeRecursiveConfig ::
  Config ->
  Path ->
  IO (PathSizeResult Natural)
pathSizeRecursiveConfig cfg path =
  findLargestPaths cfg path <&> \case
    PathSizeSuccess (MkSubPathData (pd :<|| _)) -> PathSizeSuccess $ pd ^. #size
    PathSizePartial errs (MkSubPathData (pd :<|| _)) -> PathSizePartial errs (pd ^. #size)

-- | Given a path, associates all subpaths to their size, recursively.
-- The searching is performed sequentially.
--
-- @since 0.1
pathDataRecursiveSync ::
  Config ->
  Path ->
  IO (Seq PathE, PathTree)
pathDataRecursiveSync = pathDataRecursive traverse

-- | Like 'pathDataRecursive', but each recursive call is run in its own
-- thread.
--
-- @since 0.1
pathDataRecursiveAsync ::
  Config ->
  Path ->
  IO (Seq PathE, PathTree)
pathDataRecursiveAsync = pathDataRecursive Async.mapConcurrently

-- | Like 'pathDataRecursiveAsync', but runs with a thread pool.
--
-- @since 0.1
pathDataRecursiveAsyncPooled ::
  Config ->
  Path ->
  IO (Seq PathE, PathTree)
pathDataRecursiveAsyncPooled = pathDataRecursive Async.pooledMapConcurrently

-- | Given a path, associates all subpaths to their size, recursively.
-- The searching is performed via the parameter traversal.
--
-- @since 0.1
pathDataRecursive ::
  -- | Traversal function.
  (forall a b t. (Traversable t) => (a -> IO b) -> t a -> IO (t b)) ->
  -- | The config.
  Config ->
  -- | Start path.
  Path ->
  IO (Seq PathE, PathTree)
pathDataRecursive traverseFn cfg = go 0
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

    shouldSkip =
      if cfg ^. #searchAll
        then skipExcluded . FP.takeFileName
        else (\p -> hidden p || skipExcluded p) . FP.takeFileName

    -- Base recursive function. If the path is determined to be a symlink or
    -- file, calculates the size. If it is a directory, we recursively call
    -- go on all subpaths.
    go ::
      Natural ->
      Path ->
      IO (Seq PathE, PathTree)
    go !depth path =
      calcTree `catchAny` \e ->
        -- Save exceptions. Because we are using effect classes like
        -- MonadPathReader, we may end up with an ExceptionCS i.e. it includes
        -- a CallStack. We don't want that here, so throw it away.
        pure ([MkPathE path (displayNoCS e)], emptyPathTree path)
      where
        -- Perform actual calculation.
        calcTree :: IO (Seq PathE, PathTree)
        calcTree = do
          -- 1. Symlinks
          --
          -- NOTE: Need to handle symlinks separately so that we:
          --   a. Do not chase.
          --   b. Ensure we call the right size function (getFileSize
          --      errors on dangling symlinks since it operates on the target).
          isSymLink <- pathIsSymbolicLink path
          if isSymLink
            then ([],) <$> calcSymLink path
            else do
              -- 2. Directories
              isDir <- doesDirectoryExist path
              if isDir
                then do
                  paths <- filter (not . shouldSkip) <$> listDirectory path
                  subTreesErrs <-
                    traverseFn
                      (go (depth + 1) . (path </>))
                      (Seq.fromList paths)
                  let (errs, subTrees) = flattenSeq subTreesErrs
                  -- Add the cost of the directory itself.
                  dirSize <- getFileSize path
                  let (!subSize, !numFiles, !subDirs) = sumTrees subTrees
                      !numDirectories = subDirs + 1
                      !size = dirSizeFn dirSize subSize
                      -- Do not report subpaths if the depth is exceeded.
                      subTrees'
                        | depthExceeded depth = []
                        | otherwise = subTrees
                  pure
                    ( errs,
                      MkPathData
                        { path,
                          size,
                          numFiles,
                          numDirectories
                        }
                        :^| subTrees'
                    )
                else -- 3. Files
                  ([],) <$> calcFile path

sumTrees :: Seq PathTree -> (Integer, Integer, Integer)
sumTrees = foldl' (\acc t -> acc `addTuple` getSum t) (0, 0, 0)

getSum :: PathTree -> (Integer, Integer, Integer)
getSum (MkPathData {size, numFiles, numDirectories} :^| _) =
  (size, numFiles, numDirectories)

addTuple ::
  (Integer, Integer, Integer) -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
addTuple (!a, !b, !c) (!a', !b', !c') = (a + a', b + b', c + c')

-- NOTE: Detects hidden paths via a rather crude 'dot' check, with an
-- exception for the current directory ./.
--
-- TODO: Update for OsPath compat
hidden :: Path -> Bool
hidden ('.' : '/' : _) = False
hidden ('.' : _) = True
hidden _ = False

calcSymLink :: Path -> IO PathTree
calcSymLink = calcSizeFn (fmap fromIntegral . getSymLinkSize)
  where
    getSymLinkSize =
      fmap Posix.fileSize . Posix.getSymbolicLinkStatus

calcFile :: Path -> IO PathTree
calcFile = calcSizeFn getFileSize

calcSizeFn ::
  (Path -> IO Integer) ->
  Path ->
  IO PathTree
calcSizeFn sizeFn path =
  sizeFn path <&> \size ->
    MkPathData
      { path = path,
        size,
        numFiles = 1,
        numDirectories = 0
      }
      :^| []

flattenSeq :: Seq (Seq a, b) -> (Seq a, Seq b)
flattenSeq Empty = (Empty, Empty)
flattenSeq ((xs, y) :<| zs) = (xs <> xs', y <| ys)
  where
    (xs', ys) = flattenSeq zs
{-# INLINEABLE flattenSeq #-}
