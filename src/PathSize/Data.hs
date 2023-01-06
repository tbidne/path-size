{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Supplies types and related functions.
--
-- @since 0.1
module PathSize.Data
  ( -- * Base types
    PathData (..),
    PathSizeResult (..),
    _PathSizeSuccess,
    _PathSizePartial,
    _PathSizeFailure,

    -- ** Aggregate paths
    NonEmptySeq (.., (:||)),
    unNonEmptySeq,
    PathTree (..),
    takeLargestN,
    SubPathData,
    unSubPathData,
    mkSubPathData,
    display,

    -- * Config
    Config (..),
    Strategy (..),
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.DeepSeq (NFData)
import Data.Bytes
  ( Bytes (MkBytes),
    FloatingFormatter (MkFloatingFormatter),
    Normalize (normalize),
    Size (B),
  )
import Data.Bytes qualified as Bytes
import Data.Foldable (Foldable (foldl'))
import Data.HashSet (HashSet)
import Data.HashSet qualified as HSet
import Data.Ord (Down (Down))
import Data.Sequence (Seq ((:<|)), (<|))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Optics.Core (Lens', (^.))
import Optics.TH (makeFieldLabelsNoPrefix, makePrisms)
import PathSize.Exception (PathE (..))

-- | Non-empty 'Seq'.
--
-- @since 0.1
newtype NonEmptySeq a = MkNonEmptySeq (a, Seq a)
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance Foldable NonEmptySeq where
  foldr f acc (x :|| xs) = foldr f acc (x <| xs)

-- | Pattern synonym for 'NonEmptySeq'.
--
-- @since 0.1
pattern (:||) :: a -> Seq a -> NonEmptySeq a
pattern pd :|| xs <- MkNonEmptySeq (pd, xs)
  where
    pd :|| xs = MkNonEmptySeq (pd, xs)

{-# COMPLETE (:||) #-}

-- | @since 0.1
unNonEmptySeq :: NonEmptySeq a -> Seq a
unNonEmptySeq (x :|| xs) = x <| xs

-- | Associates a path to its total (recursive) size in the file-system.
--
-- @since 0.1
data PathData = MkPathData
  { -- | Path.
    --
    -- @since 0.1
    path :: !FilePath,
    -- | Size in bytes.
    --
    -- @since 0.1
    size :: !Natural,
    -- | Number of files.
    --
    -- @since 0.1
    numFiles :: !Natural,
    -- | Number of directories.
    --
    -- @since 0.1
    numDirectories :: !Natural
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''PathData

-- | Result of running a path-size computation with multiple notions of
-- failure.
--
-- @since 0.1
data PathSizeResult a
  = -- | Successfully computed @a@.
    --
    -- @since 0.1
    PathSizeSuccess !a
  | -- | Computed @a@ with some errors.
    --
    -- @since 0.1
    PathSizePartial !(NonEmptySeq PathE) !a
  | -- | Failed computing @a@.
    --
    -- @since 0.1
    PathSizeFailure !(NonEmptySeq PathE)
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Functor,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makePrisms ''PathSizeResult

-- | Given a path, represents the directory tree, with each subpath
-- associated to its size. This structure is essentially a rose tree.
--
-- @since 0.1
data PathTree
  = Nil
  | Node !PathData !(Seq PathTree)
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

pathTreeToSeq :: PathTree -> Seq PathData
pathTreeToSeq (Node x subTrees) = x <| (subTrees >>= pathTreeToSeq)
pathTreeToSeq Nil = []

-- | A flattened and sorted representation of 'PathTree'. Contains at least
-- one element.
--
-- @since 0.1
newtype SubPathData = UnsafeSubPathData (NonEmptySeq PathData)
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
unSubPathData :: SubPathData -> NonEmptySeq PathData
unSubPathData (UnsafeSubPathData sbd) = sbd

-- | Creates a 'SubPathData' from a 'PathTree'.
--
-- @since 0.1
mkSubPathData :: PathTree -> Maybe SubPathData
mkSubPathData Nil = Nothing
mkSubPathData node@(Node _ _) = case sort (pathTreeToSeq node) of
  (first :<| rest) -> Just $ UnsafeSubPathData (first :|| rest)
  -- HACK: This should be impossible as sorting preserves size...
  _ -> Nothing

-- | Returns a 'Seq' representation of 'SubPathData'.
--
-- @since 0.1
subPathDataToSeq :: SubPathData -> Seq PathData
subPathDataToSeq (UnsafeSubPathData (pd :|| xs)) = pd <| xs

-- NOTE: Annoyingly, this sort seems to cost quite a bit of performance over
-- the previous (Down . view #size). It is now applying an additional sort
-- to the path. This was done initially to make testing easier (stable order
-- for paths w/ identical sizes), but the added determinism + usability
-- seems worth it.

-- | Sorts the path size.
--
-- @since 0.1
sort :: Seq PathData -> Seq PathData
sort = Seq.sortOn (Down . \(MkPathData p s _ _) -> (s, p))

-- | Retrieves the largest N paths.
--
-- @since 0.1
takeLargestN :: Natural -> PathTree -> Maybe SubPathData
takeLargestN _ Nil = Nothing
takeLargestN n node@(Node _ _) = case Seq.take (fromIntegral n) (sort (pathTreeToSeq node)) of
  (first :<| rest) -> Just $ UnsafeSubPathData (first :|| rest)
  -- NOTE: Should only happen if n == 0
  _ -> Nothing

-- | Displays the data.
--
-- @since 0.1
display :: Bool -> SubPathData -> Text
display revSort = showList' . subPathDataToSeq
  where
    showList' :: Seq PathData -> Text
    showList' = TL.toStrict . TLB.toLazyText . foldSeq go ""
    go (MkPathData {path, size, numFiles, numDirectories}) acc =
      mconcat
        [ TLB.fromString path,
          ": ",
          TLB.fromLazyText $ TL.fromStrict $ formatSize size,
          ", Directories: ",
          TLB.fromString $ show numDirectories,
          ", Files: ",
          TLB.fromString $ show numFiles,
          "\n",
          acc
        ]
    formatSize :: Natural -> Text
    formatSize =
      Bytes.formatSized
        (MkFloatingFormatter (Just 2))
        Bytes.sizedFormatterUnix
        . normalize
        . MkBytes @B
        . fromIntegral @_ @Double
    foldSeq
      | revSort = foldl' . flip
      | otherwise = foldr

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
    numPaths :: !(Maybe Natural),
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
