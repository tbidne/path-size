{-# LANGUAGE UndecidableInstances #-}

-- | Supplies PathSize 'Config'.
--
-- @since 0.1
module PathSize.Data.Config
  ( Config (..),
    defaultConfig,
    Strategy (..),
    _Sync,
    _Async,
    _AsyncPool,
  )
where

import Data.HashSet (HashSet)
import Data.HashSet qualified as HSet
import GHC.Natural (Natural)
import Numeric.Data.Positive (Positive)
import Optics.Core
  ( A_Lens,
    LabelOptic (labelOptic),
    Prism',
    lensVL,
    prism,
  )
import PathSize.Data.Config.TH (defaultNumPaths)

-- | Describes the path search strategy.
--
-- @since 0.1
data Strategy
  = -- | No threads.
    --
    -- @since 0.1
    Sync
  | -- | Lightweight threads.
    --
    -- @since 0.1
    Async
  | -- | Uses a thread pool.
    --
    -- @since 0.1
    AsyncPool
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
_Sync :: Prism' Strategy ()
_Sync =
  prism
    (const Sync)
    ( \x -> case x of
        Sync -> Right ()
        _ -> Left x
    )
{-# INLINE _Sync #-}

-- | @since 0.1
_Async :: Prism' Strategy ()
_Async =
  prism
    (const Async)
    ( \x -> case x of
        Async -> Right ()
        _ -> Left x
    )
{-# INLINE _Async #-}

-- | @since 0.1
_AsyncPool :: Prism' Strategy ()
_AsyncPool =
  prism
    (const AsyncPool)
    ( \x -> case x of
        AsyncPool -> Right ()
        _ -> Left x
    )
{-# INLINE _AsyncPool #-}

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
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "searchAll" k Config Config a b
  where
  labelOptic = lensVL $ \f (MkConfig _searchAll _maxDepth _exclude _filesOnly _numPaths _strategy) ->
    fmap (\searchAll' -> MkConfig searchAll' _maxDepth _exclude _filesOnly _numPaths _strategy) (f _searchAll)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe Natural, b ~ Maybe Natural) =>
  LabelOptic "maxDepth" k Config Config a b
  where
  labelOptic = lensVL $ \f (MkConfig _searchAll _maxDepth _exclude _filesOnly _numPaths _strategy) ->
    fmap (\maxDepth' -> MkConfig _searchAll maxDepth' _exclude _filesOnly _numPaths _strategy) (f _maxDepth)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ HashSet FilePath, b ~ HashSet FilePath) =>
  LabelOptic "exclude" k Config Config a b
  where
  labelOptic = lensVL $ \f (MkConfig _searchAll _maxDepth _exclude _filesOnly _numPaths _strategy) ->
    fmap (\exclude' -> MkConfig _searchAll _maxDepth exclude' _filesOnly _numPaths _strategy) (f _exclude)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "filesOnly" k Config Config a b
  where
  labelOptic = lensVL $ \f (MkConfig _searchAll _maxDepth _exclude _filesOnly _numPaths _strategy) ->
    fmap (\filesOnly' -> MkConfig _searchAll _maxDepth _exclude filesOnly' _numPaths _strategy) (f _filesOnly)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe (Positive Int), b ~ Maybe (Positive Int)) =>
  LabelOptic "numPaths" k Config Config a b
  where
  labelOptic = lensVL $ \f (MkConfig _searchAll _maxDepth _exclude _filesOnly _numPaths _strategy) ->
    fmap (\numPaths' -> MkConfig _searchAll _maxDepth _exclude _filesOnly numPaths' _strategy) (f _numPaths)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Strategy, b ~ Strategy) =>
  LabelOptic "strategy" k Config Config a b
  where
  labelOptic = lensVL $ \f (MkConfig _searchAll _maxDepth _exclude _filesOnly _numPaths _strategy) ->
    fmap (MkConfig _searchAll _maxDepth _exclude _filesOnly _numPaths) (f _strategy)
  {-# INLINE labelOptic #-}

-- |
--
-- @
-- MkConfig
--   { searchAll = True,
--     maxDepth = Nothing,
--     exclude = [],
--     filesOnly = False,
--     numPaths = Just 10,
--     strategy = Async
--   }
-- @
--
-- @since 0.1
defaultConfig :: Config
defaultConfig =
  MkConfig
    { searchAll = True,
      maxDepth = Nothing,
      exclude = HSet.empty,
      filesOnly = False,
      numPaths = Just defaultNumPaths,
      strategy = Async
    }
