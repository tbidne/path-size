{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Supplies types and related functions.
--
-- @since 0.1
module PathSize.Data
  ( -- * Base types
    PathData (..),

    -- ** Aggregate paths
    PathTree (..),
    takeLargestN,
    SubPathData (MkSubPathData),
    mkSubPathData,
    toSeq,
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
import Data.Sequence (Seq, (<|))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Optics.Core (Lens', view, (^.))
import Optics.TH (makeFieldLabelsNoPrefix)

-- | Associates a path to its total (recursive) size in the file-system.
-- The 'Ord' instance compares the fields in the following order:
--
-- @[size, path, numFiles, numDirectories]@
--
-- ==== __Examples__
--
-- >>> MkPathData "a" 1 0 0 <= MkPathData "a" 2 0 0
-- True
--
-- >>> MkPathData "b" 1 0 0 <= MkPathData "a" 1 0 0
-- False
--
-- @since 0.1
data PathData = MkPathData
  { path :: !FilePath,
    size :: !Natural,
    numFiles :: !Natural,
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
instance Ord PathData where
  (<=) = lteFields
    [ MkOrdF (view #size),
      MkOrdF (view #path),
      MkOrdF (view #numFiles),
      MkOrdF (view #numDirectories)
    ]

data OrdF a = forall b. Ord b => MkOrdF (a -> b)

lteFields :: [OrdF a] -> a -> a -> Bool
lteFields [] _ _ = True
lteFields (MkOrdF f : fs) x y = case compare (f x) (f y) of
  EQ -> lteFields fs x y
  LT -> True
  GT -> False

-- | @since 0.1
makeFieldLabelsNoPrefix ''PathData

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

-- | A flattened and sorted representation of 'PathTree'.
--
-- @since 0.1
newtype SubPathData = UnsafeSubPathData (Seq PathData)
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

-- | Pattern synonym for 'SubPathData'.
--
-- @since 0.1
pattern MkSubPathData :: Seq PathData -> SubPathData
pattern MkSubPathData xs <- UnsafeSubPathData xs
  where
    MkSubPathData xs = UnsafeSubPathData $ sort xs

{-# COMPLETE MkSubPathData #-}

-- | Creates a 'SubPathData' from a 'PathTree'.
--
-- @since 0.1
mkSubPathData :: PathTree -> SubPathData
mkSubPathData = UnsafeSubPathData . sort . pathTreeToSeq

-- | Returns a 'Seq' representation of 'SubPathData'.
--
-- @since 0.1
toSeq :: SubPathData -> Seq PathData
toSeq (UnsafeSubPathData xs) = xs

-- | Sorts the path size.
--
-- @since 0.1
sort :: Seq PathData -> Seq PathData
sort = Seq.sortOn Down

-- | Retrieves the largest N paths.
--
-- @since 0.1
takeLargestN :: Natural -> PathTree -> SubPathData
takeLargestN n =
  UnsafeSubPathData
    . Seq.take (fromIntegral n)
    . sort
    . pathTreeToSeq

-- | Displays the data.
--
-- @since 0.1
display :: Bool -> SubPathData -> Text
display revSort = showList' . toSeq
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
