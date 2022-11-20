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

-- TODO: Configuration options to consider:
--
-- 1. Depth: limit the depth for the data we hold. That is, past some d, do not
--    store any paths lower than d. Note we still have to recurse to the end
--    to get accurate sizes. We can experiment with a utility like du,
--    to see if there is any benefit to delegating that final calculation to
--    to du rather than doing it ourselves directly.

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
  MkPathSizeConfig a b c d e <> MkPathSizeConfig a' b' c' d' e' =
    MkPathSizeConfig
      (a <|> a')
      (HSet.union b b')
      (c || c')
      (d || d')
      (e <> e')

-- | @since 0.1
instance Monoid PathSizeConfig where
  mempty = MkPathSizeConfig empty HSet.empty False False mempty
