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

-- TODO: Configuration options to consider:
--
-- 1. Depth: limit the depth for the data we hold. That is, past some d, do not
--    store any paths lower than d. Note we still have to recurse to the end
--    to get accurate sizes. We can experiment with a utility like du,
--    to see if there is any benefit to delegating that final calculation to
--    to du rather than doing it ourselves directly.
--
-- 2. Exclude: skip given directories (HashSet), files.

-- | @since 0.1
data PathSizeConfig = MkPathSizeConfig
  { -- | The number of paths to return.
    --
    -- @since 0.1
    numPaths :: !(Maybe Natural),
    -- | Whether to search hidden files/directories.
    --
    -- @since 0.1
    searchAll :: !Bool,
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
  MkPathSizeConfig a b c <> MkPathSizeConfig a' b' c' =
    MkPathSizeConfig (a <|> a') (b || b') (c <> c')

-- | @since 0.1
instance Monoid PathSizeConfig where
  mempty = MkPathSizeConfig empty False mempty
