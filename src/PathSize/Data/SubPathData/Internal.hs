{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal module for 'SubPathData'. Provides the invariant-breaking
-- data constructor for 'SubPathData'.
--
-- @since 0.1
module PathSize.Data.SubPathData.Internal
  ( SubPathData (.., MkSubPathData),
    unSubPathData,
    mkSubPathData,
    takeLargestN,
    display,
  )
where

import Control.DeepSeq (NFData)
import Data.Bytes
  ( Bytes (MkBytes),
    FloatingFormatter (MkFloatingFormatter),
    Normalize (normalize),
    Size (B),
  )
import Data.Bytes qualified as Bytes
#if !MIN_VERSION_base(4, 20, 0)
import Data.Foldable (Foldable (foldl'))
#endif
import Data.Ord (Down (Down))
import Data.Sequence (Seq ((:<|)), (<|))
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Effects.FileSystem.Utils (OsPath)
import Effects.FileSystem.Utils qualified as FsUtils
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.Records (HasField (getField))
import GHC.Stack (HasCallStack)
import Numeric.Data.Positive (Positive (MkPositive))
import Optics.Core (A_Getter, LabelOptic (labelOptic), to)
import PathSize.Data.PathData
  ( PathData
      ( MkPathData,
        numDirectories,
        numFiles,
        path,
        size
      ),
    natify,
  )
import PathSize.Data.PathTree (PathTree, pathTreeToSeq)

-- | A flattened and sorted representation of 'PathTree'. Contains at least
-- one element.
--
-- @since 0.1
newtype SubPathData = UnsafeSubPathData (NESeq (PathData Natural))
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
instance HasField "unSubPathData" SubPathData (NESeq (PathData Natural)) where
  getField (UnsafeSubPathData xs) = xs

-- NOTE: This is hand-written because we want to use labeled optics and the
-- obvious strategy:
--
-- 1. Record selector unSubPathData
-- 2. TH generation
--
-- will require us to make the ordinary unSubPathData eliminator into an
-- unsafe record updater, which we do not want.

-- | @since 0.1
instance
  LabelOptic
    "unSubPathData"
    A_Getter
    SubPathData
    SubPathData
    (NESeq (PathData Natural))
    (NESeq (PathData Natural))
  where
  labelOptic = to (\(UnsafeSubPathData sbd) -> sbd)

-- | Pattern synonym for 'SubPathData'. Note that construction sorts the
-- underlying 'NESeq', so it is not constant.
--
-- @since 0.1
pattern MkSubPathData :: NESeq (PathData Natural) -> SubPathData
pattern MkSubPathData sbd <- UnsafeSubPathData sbd
  where
    MkSubPathData sbd = UnsafeSubPathData (sortNESeq False sbd)

{-# COMPLETE MkSubPathData #-}

-- | @since 0.1
unSubPathData :: SubPathData -> NESeq (PathData Natural)
unSubPathData (UnsafeSubPathData sbd) = sbd

-- | Creates a 'SubPathData' from a 'PathTree'.
--
-- @since 0.1
mkSubPathData :: Bool -> PathTree -> SubPathData
mkSubPathData stableSort tree = UnsafeSubPathData (natify first :<|| fmap natify rest)
  where
    first :<|| rest = sortSeq stableSort (pathTreeToSeq tree)

-- | Returns a 'Seq' representation of 'SubPathData'.
--
-- @since 0.1
subPathDataToSeq :: SubPathData -> Seq (PathData Natural)
subPathDataToSeq (UnsafeSubPathData (pd :<|| xs)) = pd <| xs

-- NOTE: Annoyingly, this sort seems to cost quite a bit of performance over
-- the previous (Down . view #size). It is now applying an additional sort
-- to the path. This was done initially to make testing easier (stable order
-- for paths w/ identical sizes), but the added determinism + usability
-- seems worth it.

-- | Sorts the path size.
--
-- @since 0.1
sortSeq :: (Ord a) => Bool -> NESeq (PathData a) -> NESeq (PathData a)
sortSeq False = NESeq.sortOn pathDataSizeOrd
sortSeq True = NESeq.sortOn pathDataSizePathOrd
{-# INLINEABLE sortSeq #-}

sortNESeq :: (Ord a) => Bool -> NESeq (PathData a) -> NESeq (PathData a)
sortNESeq False = NESeq.sortOn pathDataSizeOrd
sortNESeq True = NESeq.sortOn pathDataSizePathOrd
{-# INLINEABLE sortNESeq #-}

pathDataSizeOrd :: PathData a -> Down a
pathDataSizeOrd = Down . (.size)
{-# INLINEABLE pathDataSizeOrd #-}

pathDataSizePathOrd :: PathData a -> Down (a, OsPath)
pathDataSizePathOrd = Down . \(MkPathData p s _ _) -> (s, p)
{-# INLINEABLE pathDataSizePathOrd #-}

{- HLINT ignore takeLargestN "Redundant bracket" -}

-- | Retrieves the largest N paths.
--
-- @since 0.1
takeLargestN :: (HasCallStack) => Bool -> Positive Int -> PathTree -> SubPathData
takeLargestN stableSort (MkPositive n) tree = case NESeq.take n sorted of
  (first :<| rest) -> UnsafeSubPathData (natify first :<|| fmap natify rest)
  -- NOTE: Should only happen if n == 0
  _ ->
    error $
      mconcat
        [ "[PathSize.Data.SubPathData.Internal.takeLargestN]: ",
          "impossible, returned empty Seq for i = ",
          show n,
          ", tree = ",
          show tree
        ]
  where
    sorted = sortSeq stableSort (pathTreeToSeq tree)

-- | Displays the data.
--
-- @since 0.1
display :: Bool -> SubPathData -> Text
display revSort = showList' . subPathDataToSeq
  where
    showList' :: Seq (PathData Natural) -> Text
    showList' = TL.toStrict . TLB.toLazyText . foldSeq go ""
    go (MkPathData {path, size, numFiles, numDirectories}) acc =
      mconcat
        [ TLB.fromString $ FsUtils.decodeOsToFpShow path,
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
