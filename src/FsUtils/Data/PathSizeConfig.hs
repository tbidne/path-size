{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides configuration for size functions.
--
-- @since 0.1
module FsUtils.Data.PathSizeConfig
  ( PathSizeConfig (..),
    Strategy (..),
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.HashSet (HashSet)
import Data.HashSet qualified as HSet
import GHC.Natural (Natural)
import Optics.TH (makeFieldLabelsNoPrefix)

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
data PathSizeConfig = MkPathSizeConfig
  { -- | The number of paths to return.
    --
    -- @since 0.1
    numPaths :: !(Maybe Natural),
    -- | Paths to skip.
    --
    -- @since 0.1
    exclude :: !(HashSet FilePath),
    -- | Whether to search hidden files/directories.
    --
    -- @since 0.1
    searchAll :: !Bool,
    -- | Whether to limit our search to just files.
    --
    -- @since 0.1
    filesOnly :: !Bool,
    -- | The depth limit of our search. Note that we still need to fully
    -- traverse the file system to get accurate data; this argument merely
    -- affects what is reported i.e. any depths > d are implicitly included
    -- in parent directories, but not directly.
    maxDepth :: !(Maybe Natural),
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
makeFieldLabelsNoPrefix ''PathSizeConfig

-- | @since 0.1
instance Semigroup PathSizeConfig where
  MkPathSizeConfig a b c d e f <> MkPathSizeConfig a' b' c' d' e' f' =
    MkPathSizeConfig
      (a <|> a')
      (HSet.union b b')
      (c || c')
      (d || d')
      (e <|> e')
      (f <> f')

-- | @since 0.1
instance Monoid PathSizeConfig where
  mempty = MkPathSizeConfig empty HSet.empty False False empty mempty
