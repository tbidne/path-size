{-# LANGUAGE CPP #-}

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

    -- ** Display
    DisplayConfig (..),
    SPD.defaultDisplayConfig,
    DisplayFormat (..),
    SPD.display,

    -- * Errors
    PathE (..),
  )
where

import Control.Exception.Utils (trySync)
import Control.Monad.Catch (MonadCatch)
import Data.Sequence (Seq (Empty, (:<|)))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Data.Word (Word16)
import Effects.Concurrent.Async (MonadAsync)
import Effects.Concurrent.Async qualified as Async
import Effects.FileSystem.PathReader (MonadPathReader)
import Effects.FileSystem.PathReader qualified as RDir
import FileSystem.OsPath (OsPath, decodeLenient, (</>))
import GHC.Stack (HasCallStack)
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
    defaultNumPathsSize,
  )
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
import PathSize.Data.SubPathData.Internal
  ( DisplayConfig (MkDisplayConfig, color, format, reverseSort),
    DisplayFormat (DisplayFormatSingle, DisplayFormatTabular),
    SubPathData (UnsafeSubPathData),
  )
import PathSize.Exception (PathE (MkPathE))
import PathSize.Utils (MonadPosixC)
import PathSize.Utils qualified as Utils
import System.FilePath.Glob qualified as Glob
import System.OsPath qualified as FP
import System.PosixCompat.Files qualified as PCompat.Files

{- HLINT ignore "Redundant bracket" -}

-- | Given a path, finds the size of all subpaths, recursively.
--
-- @since 0.1
findLargestPaths ::
  ( HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadPathReader m,
    MonadPosixC m
  ) =>
  -- | Configuration.
  Config ->
  -- | OsPath to search.
  OsPath ->
  -- | The results.
  m (PathSizeResult SubPathData)
findLargestPaths cfg = (fmap . fmap) takeLargestN . f cfg
  where
    f = case cfg.strategy of
      Sync -> pathDataRecursiveSync
      Async -> pathDataRecursiveAsync
      AsyncPool -> pathDataRecursiveAsyncPool
    takeLargestN =
      maybe
        (SPD.mkSubPathData cfg.stableSort)
        (SPD.takeLargestN cfg.stableSort)
        (cfg.numPaths)
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
    MonadPosixC m
  ) =>
  OsPath ->
  m (PathSizeResult Integer)
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
    MonadPosixC m
  ) =>
  Config ->
  OsPath ->
  m (PathSizeResult Integer)
pathSizeRecursiveConfig cfg = (fmap . fmap) getSize . findLargestPaths cfg
  where
    getSize (UnsafeSubPathData (pd :<|| _)) = pd.size
{-# INLINEABLE pathSizeRecursiveConfig #-}

-- | Given a path, associates all subpaths to their size, recursively.
-- The searching is performed sequentially.
--
-- @since 0.1
pathDataRecursiveSync ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadPosixC m
  ) =>
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
    MonadPosixC m
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
    MonadPosixC m
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
{-# INLINEABLE pathDataRecursive #-}
pathDataRecursive ::
  forall m.
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadPosixC m
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
    excluded = cfg.exclude

    skipExcluded :: OsPath -> Bool
    skipExcluded p = any (\pat -> Glob.match pat (decodeLenient p)) excluded

    -- NOTE: [Directory sizes]
    dirSizeFn
      -- filesOnly -> directories are set to size 0
      | cfg.filesOnly = \_ _ -> 0
      -- ignoreDirIntrinsicSize -> directories are set to subfiles size;
      -- intrinsic size of the dir itself is ignored. This relies on the
      -- _first_ param being the subfiles size.
      | cfg.ignoreDirIntrinsicSize = const
      | otherwise = (+)

    -- NOTE: If a maxDepth is given, we do not include paths that exceed
    -- the depth. Note that they are still included in size calculation for
    -- parent directories.
    depthExceeded = case cfg.maxDepth of
      Nothing -> const False
      Just d -> (>= d)

    shouldSkip = case (cfg.searchAll, null excluded) of
      -- 1. Search all and no excluded paths: no checks
      (True, True) -> const False
      -- 2. No search all: check hidden
      (False, True) -> Utils.hidden . FP.takeFileName
      -- 3. Some excluded paths: check excluded
      (True, False) -> skipExcluded
      -- 4. No search all and some excluded paths: check hidden and excluded
      (False, False) -> (\p -> Utils.hidden (FP.takeFileName p) || skipExcluded p)

    -- Base recursive function. If the path is determined to be a symlink or
    -- file, calculates the size. If it is a directory, we recursively call
    -- tryGo on all subpaths.

    tryGo ::
      (HasCallStack) =>
      Word16 ->
      OsPath ->
      m (PathSizeResult PathTree)
    tryGo !depth !path =
      trySync (Utils.getFileStatus path) >>= \case
        Left ex -> pure $ mkPathE path ex
        Right stats -> do
          -- see NOTE: [Efficient Int Type]
          let size = fromIntegral $ PCompat.Files.fileSize stats
          -- Treat all non-directories identically. getFileStatus already
          -- handles symbolic links for us (by using getSymbolicLinkStatus),
          -- There are still other file types e.g. named pipes, but I see no
          -- reason to differentiate here i.e. the only choice we have to make
          -- is directory vs. non-directory.
          if PCompat.Files.isDirectory stats
            then tryCalcDir size path depth
            else
              pure $
                PathSizeSuccess $
                  PathTree.singleton $
                    MkPathData
                      { path,
                        size,
                        numFiles = 1,
                        numDirectories = 0
                      }

    tryCalcDir ::
      (HasCallStack) =>
      Integer ->
      OsPath ->
      Word16 ->
      m (PathSizeResult PathTree)
    tryCalcDir !dirSize !path !depth =
      trySync (filter (not . shouldSkip) <$> RDir.listDirectory path) >>= \case
        Left listDirEx -> pure $ mkPathE path listDirEx
        Right subPaths -> do
          resultSubTrees <-
            traverseFn
              (tryGo (depth + 1) . (path </>))
              (Seq.fromList subPaths)

          let (errs, subTrees) = Utils.unzipResultSeq resultSubTrees
              (# !subSize, !numFiles, !subDirs #) = PathTree.sumTrees subTrees
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
          pure $ case errs of
            Empty -> PathSizeSuccess tree
            (e :<| es) -> PathSizePartial (e :<|| es) tree
