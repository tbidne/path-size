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
import Effects.FileSystem.PathReader (MonadPathReader)
import Effects.FileSystem.PathReader qualified as RDir
import Effects.FileSystem.Utils (OsPath, (</>))
import Effects.System.PosixCompat (MonadPosixCompat)
import GHC.Natural (Natural)
import Optics.Core ((^.))
import PathSize.Data.Config
  ( Config
      ( MkConfig,
        exclude,
        filesOnly,
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

-- | Returns the total path size in bytes. Calls 'pathSizeRecursiveConfig' with
-- the following config:
--
-- @
-- MkConfig
--   { searchAll = True,
--     maxDepth = Just 0,
--     exclude = [],
--     filesOnly = False,
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
          numPaths = Just defaultNumPathsSize,
          stableSort = False,
          strategy = Async
        }

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
        else (\p -> Utils.hidden p || skipExcluded p) . FP.takeFileName

    -- Base recursive function. If the path is determined to be a symlink or
    -- file, calculates the size. If it is a directory, we recursively call
    -- tryGo on all subpaths.
    tryGo ::
      (HasCallStack) =>
      Word16 ->
      OsPath ->
      m (PathSizeResult PathTree)
    tryGo !depth !path =
      -- NOTE: Need to handle symlinks separately so that we:
      --   a. Do not chase.
      --   b. Ensure we call the right size function (getFileSize
      --      errors on dangling symlinks since it operates on the target).
      tryAny (RDir.pathIsSymbolicLink path) >>= \case
        Left isSymLinkEx -> pure $ mkPathE path isSymLinkEx
        -- 1. Symlinks
        Right True -> Utils.tryCalcSymLink path
        Right False ->
          tryAny (RDir.doesDirectoryExist path) >>= \case
            Left isDirEx -> pure $ mkPathE path isDirEx
            -- 2. Directories
            Right True -> tryCalcDir path depth
            -- 3. Files
            Right False -> Utils.tryCalcFile path

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
                  !size = dirSizeFn dirSize subSize
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
