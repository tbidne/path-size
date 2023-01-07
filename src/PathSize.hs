{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

-- | @since 0.1
module PathSize
  ( -- * Class
    MonadPathSize (..),

    -- * Types
    PathData (..),
    SubPathData,
    PathSizeResult (..),

    -- ** Configuration
    Config (..),
    Strategy (..),

    -- * High level functions
    pathSizeRecursive,
    pathSizeRecursiveConfig,
    SPD.display,
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Foldable (Foldable (foldl'))
import Data.Functor ((<&>))
import Data.HashSet qualified as HSet
import Data.Sequence (Seq (Empty, (:<|)), (<|))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NonEmptySeq ((:||)))
import Effects.FileSystem.MonadPathReader (MonadPathReader (..))
import Effects.FileSystem.Types (Path)
import Effects.MonadCallStack (HasCallStack, displayCallStack)
import GHC.Natural (Natural)
import Optics.Core ((^.))
import PathSize.Data.Config (Config (..), Strategy (..))
import PathSize.Data.PathData (PathData (..))
import PathSize.Data.PathSizeResult (PathSizeResult (..))
import PathSize.Data.PathTree (PathTree (..))
import PathSize.Data.SubPathData (SubPathData)
import PathSize.Data.SubPathData qualified as SPD
import PathSize.Exception (PathE (MkPathE))
#if MIN_VERSION_filepath(1,4,100)
import System.OsPath ((</>))
import System.OsPath qualified as FP
#else
import System.FilePath ((</>))
import System.FilePath qualified as FP
#endif
import System.Posix.Files qualified as Posix
import UnliftIO.Async qualified as Async
import UnliftIO.Exception (catchAny)

-- | Typeclass for finding a path's recursive size.
--
-- @since 0.1
class Monad m => MonadPathSize m where
  -- | Given a path, finds the size of all subpaths, recursively.
  --
  -- @since 0.1
  findLargestPaths ::
    HasCallStack =>
    -- | Configuration.
    Config ->
    -- | Path to search.
    Path ->
    -- | The results.
    m (PathSizeResult SubPathData)

-- | @since 0.1
instance MonadPathSize IO where
  findLargestPaths = findLargestPathsIO

-- | @since 0.1
instance MonadPathSize m => MonadPathSize (ReaderT env m) where
  findLargestPaths cfg = lift . findLargestPaths cfg

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
pathSizeRecursive :: MonadPathSize m => Path -> m (PathSizeResult Natural)
pathSizeRecursive = pathSizeRecursiveConfig cfg
  where
    cfg =
      MkConfig
        { searchAll = True,
          maxDepth = Just 0,
          exclude = mempty,
          filesOnly = False,
          numPaths = Just 1,
          strategy = mempty
        }

-- | Returns the total path size in bytes.
--
-- @since 0.1
pathSizeRecursiveConfig ::
  MonadPathSize m =>
  Config ->
  Path ->
  m (PathSizeResult Natural)
pathSizeRecursiveConfig cfg path =
  findLargestPaths cfg path <&> \case
    PathSizeSuccess (SPD.unSubPathData -> pd :|| _) -> PathSizeSuccess $ pd ^. #size
    PathSizePartial errs (SPD.unSubPathData -> pd :|| _) -> PathSizePartial errs (pd ^. #size)
    PathSizeFailure errs -> PathSizeFailure errs

-- | Given a path, finds the size of all subpaths, recursively.
--
-- @since 0.1
findLargestPathsIO ::
  HasCallStack =>
  -- | Configuration.
  Config ->
  -- | Path to search.
  Path ->
  -- | The results. The left element are any errors encountered, while the
  -- right element is the path size data.
  IO (PathSizeResult SubPathData)
findLargestPathsIO cfg path = do
  f cfg path <&> \case
    -- 1. Success, received data and no errors
    (Empty, nodes@(Node _ _)) -> case takeLargestN nodes of
      -- 1.a. This should only occur if numPaths == 0, as sorting a non-empty
      -- list should return a non-empty list
      Nothing -> PathSizeFailure (MkPathE path "Found 0 nodes after sorting" :|| [])
      -- 1.b. Success,
      Just sbd -> PathSizeSuccess sbd
    -- 2. Partial success, received errs and non-empty data
    (allErrs@(err :<| errs), nodes@(Node _ _)) -> case takeLargestN nodes of
      Nothing -> PathSizeFailure (MkPathE path "Found 0 nodes" :|| allErrs)
      Just sbd -> PathSizePartial (err :|| errs) sbd
    -- 3. Received errs and no data.
    (err :<| errs, Nil) -> PathSizeFailure (err :|| errs)
    -- 4. Received no errs and no data.
    (Empty, Nil) ->
      PathSizeFailure
        ( MkPathE
            path
            "Received no errors and no data. Was the top-level path excluded?"
            :|| []
        )
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

-- | Given a path, associates all subpaths to their size, recursively.
-- The searching is performed sequentially.
--
-- @since 0.1
pathDataRecursiveSync ::
  HasCallStack =>
  Config ->
  Path ->
  IO (Seq PathE, PathTree)
pathDataRecursiveSync = pathDataRecursive traverse

-- | Like 'pathDataRecursive', but each recursive call is run in its own
-- thread.
--
-- @since 0.1
pathDataRecursiveAsync ::
  HasCallStack =>
  Config ->
  Path ->
  IO (Seq PathE, PathTree)
pathDataRecursiveAsync = pathDataRecursive Async.mapConcurrently

-- | Like 'pathDataRecursiveAsync', but runs with a thread pool.
--
-- @since 0.1
pathDataRecursiveAsyncPooled ::
  HasCallStack =>
  Config ->
  Path ->
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
  Path ->
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

    goSkipHidden :: HasCallStack => Path -> IO (Seq PathE, PathTree)
    goSkipHidden = go hidden 0

    goHidden :: HasCallStack => Path -> IO (Seq PathE, PathTree)
    goHidden = go (const False) 0

    -- Base recursive function. If the path is determined to be a symlink or
    -- file, calculates the size. If it is a directory, we recursively call
    -- go on all subpaths.
    go ::
      HasCallStack =>
      (Path -> Bool) ->
      Natural ->
      Path ->
      IO (Seq PathE, PathTree)
    go skipHidden !depth path =
      -- Determine if we should skip.
      if ((\p -> skipHidden p || skipExcluded p) . FP.takeFileName) path
        then pure ([], Nil)
        else
          calcTree `catchAny` \e -> do
            -- Save exceptions
            pure ([MkPathE path (displayCallStack e)], Nil)
      where
        -- Perform actual calculation.
        calcTree :: HasCallStack => IO (Seq PathE, PathTree)
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
                  files <- listDirectory path
                  subTreesErrs <-
                    traverseFn
                      (go skipHidden (depth + 1) . (path </>))
                      (Seq.fromList files)
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
                  ([],) <$> calcFile path

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

calcSymLink :: HasCallStack => Path -> IO PathTree
calcSymLink = calcSizeFn getSymLinkSize
  where
    getSymLinkSize =
      fmap Posix.fileSize . Posix.getSymbolicLinkStatus

calcFile :: HasCallStack => Path -> IO PathTree
calcFile = calcSizeFn getFileSize

calcSizeFn ::
  (HasCallStack, Integral a) =>
  (HasCallStack => Path -> IO a) ->
  Path ->
  IO PathTree
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
