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
    SPD.display,

    -- * Errors
    PathE (..),
  )
where

import Data.Foldable (Foldable (foldl'))
import Data.Functor ((<&>))
import Data.HashSet qualified as HSet
import Data.Sequence (Seq (Empty, (:<|)))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Data.Sequence.NonEmpty qualified as NESeq
import Effects.Concurrent.Async qualified as Async
import Effects.Exception (HasCallStack, catchAny, displayNoCS, tryAny)
import Effects.FileSystem.Path (Path, (</>))
import Effects.FileSystem.PathReader (MonadPathReader (..))
import GHC.Natural (Natural)
import Optics.Core ((^.))
import PathSize.Data.Config (Config (..), Strategy (..))
import PathSize.Data.Config.TH (defaultNumPathsSize)
import PathSize.Data.PathData (PathData (..))
import PathSize.Data.PathSizeResult (PathSizeResult (..))
import PathSize.Data.PathTree (PathTree (..))
import PathSize.Data.PathTree qualified as PathTree
import PathSize.Data.SubPathData (SubPathData (MkSubPathData))
import PathSize.Data.SubPathData qualified as SPD
import PathSize.Exception (PathE (MkPathE))
#if MIN_VERSION_filepath(1,4,100)
import System.OsPath qualified as FP
#else
import System.FilePath qualified as FP
#endif
import System.PosixCompat.Files qualified as Posix

{- HLINT ignore "Redundant bracket" -}

-- | Given a path, finds the size of all subpaths, recursively.
--
-- @since 0.1
findLargestPaths ::
  (HasCallStack) =>
  -- | Configuration.
  Config ->
  -- | Path to search.
  Path ->
  -- | The results.
  IO (PathSizeResult SubPathData)
findLargestPaths cfg = (fmap . fmap) takeLargestN . f cfg
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
pathSizeRecursive :: (HasCallStack) => Path -> IO (PathSizeResult Natural)
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
  (HasCallStack) =>
  Config ->
  Path ->
  IO (PathSizeResult Natural)
pathSizeRecursiveConfig cfg path =
  findLargestPaths cfg path <&> \case
    PathSizeSuccess (MkSubPathData (pd :<|| _)) -> PathSizeSuccess $ pd ^. #size
    PathSizePartial errs (MkSubPathData (pd :<|| _)) -> PathSizePartial errs (pd ^. #size)
    PathSizeFailure errs -> PathSizeFailure errs

-- | Given a path, associates all subpaths to their size, recursively.
-- The searching is performed sequentially.
--
-- @since 0.1
pathDataRecursiveSync ::
  (HasCallStack) =>
  Config ->
  Path ->
  IO (PathSizeResult PathTree)
pathDataRecursiveSync = pathDataRecursive traverse

-- | Like 'pathDataRecursive', but each recursive call is run in its own
-- thread.
--
-- @since 0.1
pathDataRecursiveAsync ::
  (HasCallStack) =>
  Config ->
  Path ->
  IO (PathSizeResult PathTree)
pathDataRecursiveAsync = pathDataRecursive Async.mapConcurrently

-- | Like 'pathDataRecursiveAsync', but runs with a thread pool.
--
-- @since 0.1
pathDataRecursiveAsyncPooled ::
  (HasCallStack) =>
  Config ->
  Path ->
  IO (PathSizeResult PathTree)
pathDataRecursiveAsyncPooled = pathDataRecursive Async.pooledMapConcurrently

-- | Given a path, associates all subpaths to their size, recursively.
-- The searching is performed via the parameter traversal.
--
-- @since 0.1
pathDataRecursive ::
  (HasCallStack) =>
  -- | Traversal function.
  (forall a b t. (HasCallStack, Traversable t) => (a -> IO b) -> t a -> IO (t b)) ->
  -- | The config.
  Config ->
  -- | Start path.
  Path ->
  IO (PathSizeResult PathTree)
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
    -- go on all subpaths.
    tryGo ::
      (HasCallStack) =>
      Natural ->
      Path ->
      IO (PathSizeResult PathTree)
    tryGo !depth !path =
      calcTree `catchAny` \e ->
        -- Save exceptions. Because we are using effect classes like
        -- MonadPathReader, we may end up with an ExceptionCS i.e. it includes
        -- a CallStack. We don't want that here, so throw it away.
        pure $ PathSizeFailure (NESeq.singleton $ MkPathE path (displayNoCS e))
      where
        -- Perform actual calculation.
        calcTree :: (HasCallStack) => IO (PathSizeResult PathTree)
        calcTree = do
          -- 1. Symlinks
          --
          -- NOTE: Need to handle symlinks separately so that we:
          --   a. Do not chase.
          --   b. Ensure we call the right size function (getFileSize
          --      errors on dangling symlinks since it operates on the target).
          isSymLink <- pathIsSymbolicLink path
          if isSymLink
            then tryCalcSymLink path
            else do
              -- 2. Directories
              isDir <- doesDirectoryExist path
              if isDir
                then
                  tryAny (filter (not . shouldSkip) <$> listDirectory path) >>= \case
                    Left ex ->
                      pure $ PathSizeFailure (NESeq.singleton $ MkPathE path (displayNoCS ex))
                    Right subPaths -> do
                      resultSubTrees <-
                        traverseFn
                          (tryGo (depth + 1) . (path </>))
                          (Seq.fromList subPaths)
                      -- Add the cost of the directory itself.
                      dirSize <- getFileSize path
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
                      pure $ case errs of
                        Empty -> PathSizeSuccess tree
                        (e :<| es) -> PathSizePartial (e :<|| es) tree
                else -- 3. Files
                  tryCalcFile path

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

tryCalcSymLink :: (HasCallStack) => Path -> IO (PathSizeResult PathTree)
tryCalcSymLink = tryCalcSize (fmap fromIntegral . getSymLinkSize)
  where
    getSymLinkSize =
      fmap Posix.fileSize . Posix.getSymbolicLinkStatus

tryCalcFile :: (HasCallStack) => Path -> IO (PathSizeResult PathTree)
tryCalcFile = tryCalcSize getFileSize

tryCalcSize ::
  (HasCallStack) =>
  ((HasCallStack) => Path -> IO Integer) ->
  Path ->
  IO (PathSizeResult PathTree)
tryCalcSize sizeFn path = do
  tryAny (sizeFn path) <&> \case
    Left ex -> PathSizeFailure (NESeq.singleton $ MkPathE path (displayNoCS ex))
    Right size ->
      PathSizeSuccess $
        PathTree.singleton $
          MkPathData
            { path = path,
              size,
              numFiles = 1,
              numDirectories = 0
            }

flattenSeq :: Seq (PathSizeResult PathTree) -> (Seq PathE, Seq PathTree)
flattenSeq Empty = (Empty, Empty)
flattenSeq (z :<| zs) = case z of
  PathSizeSuccess tree -> (errs, tree :<| trees)
  PathSizePartial (e :<|| es) tree -> (e :<| es <> errs, tree :<| trees)
  PathSizeFailure (e :<|| es) -> (e :<| es <> errs, trees)
  where
    (errs, trees) = flattenSeq zs
{-# INLINEABLE flattenSeq #-}
