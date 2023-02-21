-- | Provides a typeclass for 'PathSize.findLargestPaths'.
--
-- @since 0.1
module Effects.FileSystem.PathSize
  ( -- * Class
    MonadPathSize (..),

    -- * Types
    PathData (..),
    SubPathData (MkSubPathData),
    PathSizeResult (..),

    -- ** Configuration
    Config (..),
    Strategy (..),

    -- * High level functions
    pathSizeRecursive,
    pathSizeRecursiveConfig,
    SPD.display,

    -- * Errors
    PathE (..),
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Functor ((<&>))
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Effects.Exception (HasCallStack)
import Effects.FileSystem.Path (Path)
import GHC.Natural (Natural)
import Optics.Core ((^.))
import PathSize qualified
import PathSize.Data.Config (Config (..), Strategy (..))
import PathSize.Data.Config.TH (defaultNumPathsSize)
import PathSize.Data.PathData (PathData (..))
import PathSize.Data.PathSizeResult (PathSizeResult (..))
import PathSize.Data.SubPathData (SubPathData (MkSubPathData))
import PathSize.Data.SubPathData qualified as SPD
import PathSize.Exception (PathE (MkPathE))

{- HLINT ignore MonadPathSize "Redundant bracket" -}

-- | Typeclass for finding a path's recursive size.
--
-- @since 0.1
class (Monad m) => MonadPathSize m where
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
    m (PathSizeResult SubPathData)

-- | @since 0.1
instance MonadPathSize IO where
  findLargestPaths = PathSize.findLargestPaths
  {-# INLINEABLE findLargestPaths #-}

-- | @since 0.1
instance (MonadPathSize m) => MonadPathSize (ReaderT env m) where
  findLargestPaths cfg = lift . findLargestPaths cfg
  {-# INLINEABLE findLargestPaths #-}

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
pathSizeRecursive :: (HasCallStack, MonadPathSize m) => Path -> m (PathSizeResult Natural)
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
{-# INLINEABLE pathSizeRecursive #-}

-- | Returns the total path size in bytes.
--
-- @since 0.1
pathSizeRecursiveConfig ::
  (HasCallStack, MonadPathSize m) =>
  Config ->
  Path ->
  m (PathSizeResult Natural)
pathSizeRecursiveConfig cfg path =
  findLargestPaths cfg path <&> \case
    PathSizeSuccess (MkSubPathData (pd :<|| _)) -> PathSizeSuccess $ pd ^. #size
    PathSizePartial errs (MkSubPathData (pd :<|| _)) -> PathSizePartial errs (pd ^. #size)
{-# INLINEABLE pathSizeRecursiveConfig #-}
