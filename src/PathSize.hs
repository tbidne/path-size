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

import Data.Functor ((<&>))
import Data.HashSet qualified as HSet
import Data.Sequence (Seq (Empty, (:<|)))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Data.Word (Word16)
import Effects.Concurrent.Async (MonadAsync)
import Effects.Concurrent.Async qualified as Async
import Effects.Exception
  ( HasCallStack,
    MonadCatch,
    tryAny,
  )
import Effects.FileSystem.PathReader (MonadPathReader, PathType (PathTypeDirectory, PathTypeFile))
import Effects.FileSystem.PathReader qualified as RDir
import Effects.FileSystem.Utils (OsPath, (</>))
import Effects.FileSystem.Utils qualified as FS.Utils
import Effects.System.PosixCompat (MonadPosixCompat, PathType (PathTypeSymbolicLink))
import Effects.System.PosixCompat qualified as Posix
import GHC.Natural (Natural)
import Optics.Core ((^.))
import PathSize.Data.Config
  ( Config
      ( MkConfig,
        exclude,
        filesOnly,
        ignoreDirIntrinsicSize,
        maxDepth,
        numPaths,
        searchAll,
        stableSort,
        strategy
      ),
    Strategy (Async, AsyncPool, Sync),
  )
import PathSize.Data.Config.TH (defaultNumPathsSize)
import PathSize.Data.PathData
  ( PathData
      ( MkPathData,
        numDirectories,
        numFiles,
        path,
        size
      ),
  )
import PathSize.Data.PathSizeResult
  ( PathSizeResult
      ( PathSizeFailure,
        PathSizePartial,
        PathSizeSuccess
      ),
    mkPathE,
  )
import PathSize.Data.PathTree (PathTree ((:^|)))
import PathSize.Data.PathTree qualified as PathTree
import PathSize.Data.SubPathData qualified as SPD
import PathSize.Data.SubPathData.Internal (SubPathData (UnsafeSubPathData))
import PathSize.Exception (PathE (MkPathE))
import PathSize.Utils qualified as Utils
import System.OsPath qualified as FP

{- HLINT ignore "Redundant bracket" -}

-- | Given a path, finds the size of all subpaths, recursively.
--
-- @since 0.1
findLargestPaths ::
  ( HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadPathReader m,
    MonadPosixCompat m
  ) =>
  -- | Configuration.
  Config ->
  -- | OsPath to search.
  OsPath ->
  -- | The results.
  m (PathSizeResult SubPathData)
findLargestPaths cfg = (fmap . fmap) takeLargestN . f cfg
  where
    f = case cfg ^. #strategy of
      Sync -> pathDataRecursiveSync
      Async -> pathDataRecursiveAsync
      AsyncPool -> pathDataRecursiveAsyncPool
    takeLargestN =
      maybe
        (SPD.mkSubPathData $ cfg ^. #stableSort)
        (SPD.takeLargestN $ cfg ^. #stableSort)
        (cfg ^. #numPaths)
{-# INLINEABLE findLargestPaths #-}

-- | Returns the total path size in bytes. Calls 'pathSizeRecursiveConfig' with
-- the following config:
--
-- @
-- MkConfig
--   { searchAll = True,
--     maxDepth = Just 0,
--     exclude = [],
--     filesOnly = False,
--     ignoreDirIntrinsicSize = False,
--     numPaths = Just 1,
--     stableSort = False,
--     strategy = Async
--   }
-- @
--
-- @since 0.1
pathSizeRecursive ::
  ( HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadPathReader m,
    MonadPosixCompat m
  ) =>
  OsPath ->
  m (PathSizeResult Natural)
pathSizeRecursive = pathSizeRecursiveConfig cfg
  where
    cfg =
      MkConfig
        { searchAll = True,
          maxDepth = Just 0,
          exclude = mempty,
          filesOnly = False,
          ignoreDirIntrinsicSize = False,
          numPaths = Just defaultNumPathsSize,
          stableSort = False,
          strategy = Async
        }
{-# INLINEABLE pathSizeRecursive #-}

-- | Returns the total path size in bytes.
--
-- @since 0.1
pathSizeRecursiveConfig ::
  ( HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadPathReader m,
    MonadPosixCompat m
  ) =>
  Config ->
  OsPath ->
  m (PathSizeResult Natural)
pathSizeRecursiveConfig cfg = (fmap . fmap) getSize . findLargestPaths cfg
  where
    getSize (UnsafeSubPathData (pd :<|| _)) = pd ^. #size
{-# INLINEABLE pathSizeRecursiveConfig #-}

-- | Given a path, associates all subpaths to their size, recursively.
-- The searching is performed sequentially.
--
-- @since 0.1
pathDataRecursiveSync ::
  (HasCallStack, MonadCatch m, MonadPathReader m, MonadPosixCompat m) =>
  Config ->
  OsPath ->
  m (PathSizeResult PathTree)
pathDataRecursiveSync = pathDataRecursive traverse
{-# INLINEABLE pathDataRecursiveSync #-}

-- | Like 'pathDataRecursive', but each recursive call is run in its own
-- thread.
--
-- @since 0.1
pathDataRecursiveAsync ::
  ( HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadPathReader m,
    MonadPosixCompat m
  ) =>
  Config ->
  OsPath ->
  m (PathSizeResult PathTree)
pathDataRecursiveAsync = pathDataRecursive Async.mapConcurrently
{-# INLINEABLE pathDataRecursiveAsync #-}

-- | Like 'pathDataRecursiveAsync', but runs with a thread pool.
--
-- @since 0.1
pathDataRecursiveAsyncPool ::
  ( HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadPathReader m,
    MonadPosixCompat m
  ) =>
  Config ->
  OsPath ->
  m (PathSizeResult PathTree)
pathDataRecursiveAsyncPool = pathDataRecursive Async.pooledMapConcurrently
{-# INLINEABLE pathDataRecursiveAsyncPool #-}

-- | Given a path, associates all subpaths to their size, recursively.
-- The searching is performed via the parameter traversal.
--
-- @since 0.1
pathDataRecursive ::
  forall m.
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadPosixCompat m
  ) =>
  -- | Traversal function.
  (forall a b t. (HasCallStack, Traversable t) => (a -> m b) -> t a -> m (t b)) ->
  -- | The config.
  Config ->
  -- | Start path.
  OsPath ->
  m (PathSizeResult PathTree)
pathDataRecursive traverseFn cfg = tryGo 0
  where
    excluded = cfg ^. #exclude
    skipExcluded p = HSet.member p excluded

    -- NOTE: [Directory sizes]
    dirSizeFn
      -- filesOnly -> directories are set to size 0
      | cfg ^. #filesOnly = \_ _ -> 0
      -- ignoreDirIntrinsicSize -> directories are set to subfiles size;
      -- intrinsic size of the dir itself is ignored. This relies on the
      -- _first_ param being the subfiles size.
      | cfg ^. #ignoreDirIntrinsicSize = const
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
        else (\p -> Utils.hidden p || skipExcluded p) . FP.takeFileName

    -- Base recursive function. If the path is determined to be a symlink or
    -- file, calculates the size. If it is a directory, we recursively call
    -- tryGo on all subpaths.
    tryGo ::
      (HasCallStack) =>
      Word16 ->
      OsPath ->
      m (PathSizeResult PathTree)
    tryGo !depth !path = do
      fp <- FS.Utils.decodeOsToFpThrowM path
      -- NOTE: Need to handle symlinks separately so that we:
      --   a. Do not chase.
      --   b. Ensure we call the right size function (getFileSize
      --      errors on dangling symlinks since it operates on the target).
      --
      -- It is tempting to use RDir.getPathType path here instead of making the
      -- doesXExist calls manually, but the former has worse performance
      -- as it also performs doesFileExist, whereas we just assume that any
      -- paths that make it through are files. At least for now this seems
      -- to work fine, and the extra call costs performance.
      tryAny (Posix.getPathType fp) >>= \case
        Right PathTypeFile -> Utils.tryCalcFile path
        Right PathTypeDirectory -> tryCalcDir path depth
        Right PathTypeSymbolicLink -> Utils.tryCalcSymLink path
        Left ex -> pure $ mkPathE path ex

    tryCalcDir :: (HasCallStack) => OsPath -> Word16 -> m (PathSizeResult PathTree)
    tryCalcDir path depth =
      tryAny (filter (not . shouldSkip) <$> RDir.listDirectory path) >>= \case
        Left listDirEx -> pure $ mkPathE path listDirEx
        Right subPaths -> do
          resultSubTrees <-
            traverseFn
              (tryGo (depth + 1) . (path </>))
              (Seq.fromList subPaths)
          -- Add the cost of the directory itself.
          tryAny (RDir.getFileSize path) <&> \case
            Left sizeErr -> mkPathE path sizeErr
            Right dirSize -> do
              let (errs, subTrees) = Utils.unzipResultSeq resultSubTrees
                  (!subSize, !numFiles, !subDirs) = PathTree.sumTrees subTrees
                  !numDirectories = subDirs + 1
                  -- NOTE: subSize needs to be the first param to corrrectly
                  -- account for ignoreDirIntrinsicSize.
                  -- See NOTE: [Directory sizes]
                  !size = dirSizeFn subSize dirSize
                  -- Do not report subpaths if the depth is exceeded.
                  subTrees'
                    | depthExceeded depth = Empty
                    | otherwise = subTrees
                  tree =
                    MkPathData
                      { path,
                        size,
                        numFiles,
                        numDirectories
                      }
                      :^| subTrees'
              case errs of
                Empty -> PathSizeSuccess tree
                (e :<| es) -> PathSizePartial (e :<|| es) tree
    {-# INLINEABLE tryGo #-}
    {-# INLINEABLE tryCalcDir #-}
{-# INLINEABLE pathDataRecursive #-}
