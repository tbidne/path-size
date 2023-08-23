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

import Control.Monad ((<=<))
import Data.Foldable (Foldable (foldl'))
import Data.Functor ((<&>))
import Data.HashSet qualified as HSet
import Data.Sequence (Seq (Empty, (:<|)))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Word (Word16)
import Effects.Concurrent.Async (MonadAsync)
import Effects.Concurrent.Async qualified as Async
import Effects.Concurrent.Thread (MonadThread)
import Effects.Exception
  ( Exception,
    HasCallStack,
    MonadCatch,
    displayNoCS,
    tryAny,
  )
import Effects.FileSystem.PathReader (MonadPathReader)
import Effects.FileSystem.PathReader qualified as RDir
import Effects.FileSystem.Utils (OsPath, fromOsPath, fromOsPathThrowM, (</>))
import Effects.IORef (MonadIORef)
import Effects.System.PosixCompat (MonadPosixCompat)
import Effects.System.PosixCompat qualified as Posix
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
  )
import PathSize.Data.PathTree (PathTree ((:^|)))
import PathSize.Data.PathTree qualified as PathTree
import PathSize.Data.SubPathData qualified as SPD
import PathSize.Data.SubPathData.Internal (SubPathData (UnsafeSubPathData))
import PathSize.Exception (PathE (MkPathE))
import System.OsPath qualified as FP
import System.PosixCompat.Files qualified as PFiles

{- HLINT ignore "Redundant bracket" -}

-- | Given a path, finds the size of all subpaths, recursively.
--
-- @since 0.1
findLargestPaths ::
  ( HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadIORef m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadThread m
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
--     strategy = Async
--   }
-- @
--
-- @since 0.1
pathSizeRecursive ::
  ( HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadIORef m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadThread m
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
    MonadIORef m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadThread m
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
    MonadIORef m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadThread m
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
        else (\p -> hidden p || skipExcluded p) . FP.takeFileName

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
        Right True -> tryCalcSymLink path
        Right False ->
          tryAny (RDir.doesDirectoryExist path) >>= \case
            Left isDirEx -> pure $ mkPathE path isDirEx
            -- 2. Directories
            Right True -> tryCalcDir path depth
            -- 3. Files
            Right False -> tryCalcFile path

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
              let (errs, subTrees) = flattenSeq resultSubTrees
                  (!subSize, !numFiles, !subDirs) = sumTrees subTrees
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

sumTrees :: Seq PathTree -> (Integer, Integer, Integer)
sumTrees = foldl' (\acc t -> acc `addTuple` getSum t) (0, 0, 0)

getSum :: PathTree -> (Integer, Integer, Integer)
getSum (MkPathData {size, numFiles, numDirectories} :^| _) =
  (size, numFiles, numDirectories)

addTuple ::
  (Integer, Integer, Integer) ->
  (Integer, Integer, Integer) ->
  (Integer, Integer, Integer)
addTuple (!a, !b, !c) (!a', !b', !c') = (a + a', b + b', c + c')

-- NOTE: Detects hidden paths via a rather crude 'dot' check, with an
-- exception for the current directory ./.
hidden :: OsPath -> Bool
hidden p = case fromOsPath p of
  Left _ -> False
  Right s -> case s of
    '.' : '/' : _ -> False
    '.' : _ -> True
    _ -> False

tryCalcSymLink ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadPosixCompat m
  ) =>
  OsPath ->
  m (PathSizeResult PathTree)
tryCalcSymLink =
  tryCalcSize
    (fmap fromIntegral . getSymLinkSize <=< fromOsPathThrowM)
  where
    getSymLinkSize = fmap PFiles.fileSize . Posix.getSymbolicLinkStatus

tryCalcFile ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m
  ) =>
  OsPath ->
  m (PathSizeResult PathTree)
tryCalcFile = tryCalcSize RDir.getFileSize

tryCalcSize ::
  (HasCallStack, MonadCatch m) =>
  ((HasCallStack) => OsPath -> m Integer) ->
  OsPath ->
  m (PathSizeResult PathTree)
tryCalcSize sizeFn path = do
  tryAny (sizeFn path) <&> \case
    Left ex -> mkPathE path ex
    Right size ->
      PathSizeSuccess $
        PathTree.singleton $
          MkPathData
            { path,
              size,
              numFiles = 1,
              numDirectories = 0
            }

flattenSeq :: Seq (PathSizeResult PathTree) -> (Seq PathE, Seq PathTree)
flattenSeq = foldl' f (Empty, Empty)
  where
    f (errs, trees) = \case
      PathSizeSuccess tree -> (errs, tree :<| trees)
      PathSizePartial (e :<|| es) tree -> (e :<| es <> errs, tree :<| trees)
      PathSizeFailure (e :<|| es) -> (e :<| es <> errs, trees)
{-# INLINEABLE flattenSeq #-}

mkPathE :: (Exception e) => OsPath -> e -> PathSizeResult a
mkPathE path = PathSizeFailure . NESeq.singleton . MkPathE path . displayNoCS
