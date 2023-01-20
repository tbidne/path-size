{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Supplies PathSize 'Config'.
--
-- @since 0.1
module PathSize.Data.Config
  ( Config (..),
    Strategy (..),
    _Sync,
    _Async,
    _AsyncPooled,
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.HashSet (HashSet)
import Data.HashSet qualified as HSet
import GHC.Natural (Natural)
import Numeric.Data.Positive (Positive)
import Optics.Core (Lens', (^.))
import Optics.TH (makeFieldLabelsNoPrefix, makePrisms)

-- | Describes the path search strategy.
--
-- @since 0.1
data Strategy
  = -- | @since 0.1
    Sync
  | -- | @since 0.1
    Async
  | -- | @since 0.1
    AsyncPooled
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
makePrisms ''Strategy

-- | @since 0.1
instance Semigroup Strategy where
  AsyncPooled <> _ = AsyncPooled
  _ <> AsyncPooled = AsyncPooled
  Async <> _ = Async
  _ <> Async = Async
  Sync <> Sync = Sync

-- | @since 0.1
instance Monoid Strategy where
  mempty = Sync

-- | @since 0.1
data Config = MkConfig
  { -- | Whether to search hidden files/directories.
    --
    -- @since 0.1
    searchAll :: !Bool,
    -- | The depth limit of our search. Note that we still need to fully
    -- traverse the file system to get accurate data; this argument merely
    -- affects what is reported i.e. any depths > d are implicitly included
    -- in parent directories, but not directly.
    maxDepth :: !(Maybe Natural),
    -- | Paths to skip.
    --
    -- @since 0.1
    exclude :: !(HashSet FilePath),
    -- | Whether to limit our search to just files.
    --
    -- @since 0.1
    filesOnly :: !Bool,
    -- | The number of paths to return.
    --
    -- @since 0.1
    numPaths :: !(Maybe (Positive Int)),
    -- | The search strategy.
    --
    -- @since 0.1
    strategy :: !Strategy
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''Config

-- | @since 0.1
instance Semigroup Config where
  lhs <> rhs =
    MkConfig
      { searchAll = mergeOr #searchAll,
        maxDepth = mergeAlt #maxDepth,
        exclude = merge HSet.union #exclude,
        filesOnly = mergeOr #filesOnly,
        numPaths = mergeAlt #numPaths,
        strategy = merge (<>) #strategy
      }
    where
      mergeAlt = merge (<|>)
      mergeOr = merge (||)
      merge ::
        (a -> a -> a) ->
        Lens' Config a ->
        a
      merge f o = (lhs ^. o) `f` (rhs ^. o)

-- | @since 0.1
instance Monoid Config where
  mempty =
    MkConfig
      { searchAll = False,
        maxDepth = empty,
        exclude = HSet.empty,
        filesOnly = False,
        numPaths = empty,
        strategy = mempty
      }
