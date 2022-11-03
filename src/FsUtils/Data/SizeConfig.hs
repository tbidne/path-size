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

-- | @since 0.1
data PathSizeConfig = MkPathSizeConfig
  { -- | The number of paths to return.
    --
    -- @since 0.1
    numPaths :: Maybe Natural,
    -- | The search strategy.
    --
    -- @since 0.1
    strategy :: Strategy
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
  MkPathSizeConfig a b <> MkPathSizeConfig a' b' =
    MkPathSizeConfig (a <|> a') (b <> b')

-- | @since 0.1
instance Monoid PathSizeConfig where
  mempty = MkPathSizeConfig empty mempty
