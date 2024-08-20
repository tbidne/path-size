{-# LANGUAGE UndecidableInstances #-}

-- | Supplies PathSize 'Config'.
--
-- @since 0.1
module PathSize.Data.Config
  ( -- * Config
    Config (..),
    defaultConfig,

    -- * Strategy
    Strategy (..),
    _Sync,
    _Async,
    _AsyncPool,

    -- * Constants
    defaultNumPaths,
    defaultNumPathsSize,
  )
where

import Data.HashSet (HashSet)
import Data.HashSet qualified as HSet
import Data.Word (Word16)
import Effects.FileSystem.OsPath (OsPath)
import Numeric.Data.Positive.Internal (Positive (UnsafePositive))
import Optics.Core
  ( A_Lens,
    LabelOptic (labelOptic),
    Prism',
    lensVL,
    prism,
  )

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
      Bounded,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Enum,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
_Sync :: Prism' Strategy ()
_Sync =
  prism
    (const Sync)
    ( \case
        Sync -> Right ()
        x -> Left x
    )
{-# INLINE _Sync #-}

-- | @since 0.1
_Async :: Prism' Strategy ()
_Async =
  prism
    (const Async)
    ( \case
        Async -> Right ()
        x -> Left x
    )
{-# INLINE _Async #-}

-- | @since 0.1
_AsyncPool :: Prism' Strategy ()
_AsyncPool =
  prism
    (const AsyncPool)
    ( \case
        AsyncPool -> Right ()
        x -> Left x
    )
{-# INLINE _AsyncPool #-}

-- | Default num paths for normal, full search.
--
-- @since 0.1
defaultNumPaths :: Positive Int
defaultNumPaths = UnsafePositive 10

-- | Default num paths for size search.
--
-- @since 0.1
defaultNumPathsSize :: Positive Int
defaultNumPathsSize = UnsafePositive 1

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
    maxDepth :: !(Maybe Word16),
    -- | Paths to skip.
    --
    -- @since 0.1
    exclude :: !(HashSet OsPath),
    -- | Whether to limit our search to just files.
    --
    -- @since 0.1
    filesOnly :: !Bool,
    -- | If active, this flag ignores the size of the directory itself e.g.
    -- 4096 bytes on a typical ext4 filesystem.
    --
    -- @since 0.1
    ignoreDirIntrinsicSize :: !Bool,
    -- | The number of paths to return.
    --
    -- @since 0.1
    numPaths :: !(Maybe (Positive Int)),
    -- | If enabled, sorts by path name after the size. This makes the sort
    -- stable, at the cost of performance.
    --
    -- @since 0.1
    stableSort :: !Bool,
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
  labelOptic =
    lensVL $
      \f
       ( MkConfig
           _searchAll
           _maxDepth
           _exclude
           _filesOnly
           _ignoreDirIntrinsicSize
           _numPaths
           _stableSort
           _strategy
         ) ->
          fmap
            ( \searchAll' ->
                MkConfig
                  searchAll'
                  _maxDepth
                  _exclude
                  _filesOnly
                  _ignoreDirIntrinsicSize
                  _numPaths
                  _stableSort
                  _strategy
            )
            (f _searchAll)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe Word16, b ~ Maybe Word16) =>
  LabelOptic "maxDepth" k Config Config a b
  where
  labelOptic =
    lensVL $
      \f
       ( MkConfig
           _searchAll
           _maxDepth
           _exclude
           _filesOnly
           _ignoreDirIntrinsicSize
           _numPaths
           _stableSort
           _strategy
         ) ->
          fmap
            ( \maxDepth' ->
                MkConfig
                  _searchAll
                  maxDepth'
                  _exclude
                  _filesOnly
                  _ignoreDirIntrinsicSize
                  _numPaths
                  _stableSort
                  _strategy
            )
            (f _maxDepth)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ HashSet OsPath, b ~ HashSet OsPath) =>
  LabelOptic "exclude" k Config Config a b
  where
  labelOptic =
    lensVL $
      \f
       ( MkConfig
           _searchAll
           _maxDepth
           _exclude
           _filesOnly
           _ignoreDirIntrinsicSize
           _numPaths
           _stableSort
           _strategy
         ) ->
          fmap
            ( \exclude' ->
                MkConfig
                  _searchAll
                  _maxDepth
                  exclude'
                  _filesOnly
                  _ignoreDirIntrinsicSize
                  _numPaths
                  _stableSort
                  _strategy
            )
            (f _exclude)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "filesOnly" k Config Config a b
  where
  labelOptic =
    lensVL $
      \f
       ( MkConfig
           _searchAll
           _maxDepth
           _exclude
           _filesOnly
           _ignoreDirIntrinsicSize
           _numPaths
           _stableSort
           _strategy
         ) ->
          fmap
            ( \filesOnly' ->
                MkConfig
                  _searchAll
                  _maxDepth
                  _exclude
                  filesOnly'
                  _ignoreDirIntrinsicSize
                  _numPaths
                  _stableSort
                  _strategy
            )
            (f _filesOnly)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "ignoreDirIntrinsicSize" k Config Config a b
  where
  labelOptic =
    lensVL $
      \f
       ( MkConfig
           _searchAll
           _maxDepth
           _exclude
           _filesOnly
           _ignoreDirIntrinsicSize
           _numPaths
           _stableSort
           _strategy
         ) ->
          fmap
            ( \ignoreDirIntrinsicSize' ->
                MkConfig
                  _searchAll
                  _maxDepth
                  _exclude
                  _filesOnly
                  ignoreDirIntrinsicSize'
                  _numPaths
                  _stableSort
                  _strategy
            )
            (f _ignoreDirIntrinsicSize)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe (Positive Int), b ~ Maybe (Positive Int)) =>
  LabelOptic "numPaths" k Config Config a b
  where
  labelOptic =
    lensVL $
      \f
       ( MkConfig
           _searchAll
           _maxDepth
           _exclude
           _filesOnly
           _ignoreDirIntrinsicSize
           _numPaths
           _stableSort
           _strategy
         ) ->
          fmap
            ( \numPaths' ->
                MkConfig
                  _searchAll
                  _maxDepth
                  _exclude
                  _filesOnly
                  _ignoreDirIntrinsicSize
                  numPaths'
                  _stableSort
                  _strategy
            )
            (f _numPaths)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "stableSort" k Config Config a b
  where
  labelOptic =
    lensVL $
      \f
       ( MkConfig
           _searchAll
           _maxDepth
           _exclude
           _filesOnly
           _ignoreDirIntrinsicSize
           _numPaths
           _stableSort
           _strategy
         ) ->
          fmap
            ( \stableSort' ->
                MkConfig
                  _searchAll
                  _maxDepth
                  _exclude
                  _filesOnly
                  _ignoreDirIntrinsicSize
                  _numPaths
                  stableSort'
                  _strategy
            )
            (f _stableSort)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Strategy, b ~ Strategy) =>
  LabelOptic "strategy" k Config Config a b
  where
  labelOptic =
    lensVL $
      \f
       ( MkConfig
           _searchAll
           _maxDepth
           _exclude
           _filesOnly
           _ignoreDirIntrinsicSize
           _numPaths
           _stableSort
           _strategy
         ) ->
          fmap
            ( MkConfig
                _searchAll
                _maxDepth
                _exclude
                _filesOnly
                _ignoreDirIntrinsicSize
                _numPaths
                _stableSort
            )
            (f _strategy)
  {-# INLINE labelOptic #-}

-- |
--
-- @
-- MkConfig
--   { searchAll = True,
--     maxDepth = Nothing,
--     exclude = [],
--     filesOnly = False,
--     ignoreDirIntrinsicSize = False,
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
      ignoreDirIntrinsicSize = False,
      numPaths = Just defaultNumPaths,
      stableSort = False,
      strategy = Async
    }
