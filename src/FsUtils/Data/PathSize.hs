{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Supplies the 'PathSize' data type and related functions.
--
-- @since 0.1
module FsUtils.Data.PathSize
  ( Path (..),
    SizedPath (..),
    PathSizeData,
    sortPathSize,
    largestN,
    displayPathSize,
    sumPathSizes,
    sumPathSize,
  )
where

import Control.DeepSeq (NFData)
import Data.Foldable (Foldable (foldl'))
import Optics.TH (makeFieldLabelsNoPrefix)
import Optics.Core (view, _2, (%))
import Data.HashSet (HashSet)
import Data.HashSet qualified as HSet
import Data.Hashable (Hashable (hashWithSalt))
import Data.List qualified as L
import Data.Ord (Down (Down))
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import GHC.Generics (Generic)
import GHC.Natural (Natural)

data Path
  = -- | @since 0.1
    Directory !FilePath
  | -- | @since 0.1
    File !FilePath
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
      Hashable,
      -- | @since 0.1
      NFData
    )

-- | @since 0.1
newtype SizedPath = MkSizedPath { unSizedPath :: (Path, Integer) }
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
makeFieldLabelsNoPrefix ''SizedPath

-- | @since 0.1
instance Hashable SizedPath where
  hashWithSalt i (MkSizedPath (p, _)) = hashWithSalt i p

-- TODO: Should this be a regular Map, using its Ord rather than sorting
-- the HashMap later? Benchmark.
--
-- Also, if we end up using a sorted map, we should consider using a Set
-- since we want to sort on the size, and at that point there's no reason
-- to prefer the map (Path -> Integer) over a set (Path, Integer)

-- | Associates paths to their sizes.
--
-- @since 0.1
type PathSizeData = HashSet SizedPath

-- | Sorts the path size.
--
-- @since 0.1
sortPathSize :: PathSizeData -> [SizedPath]
sortPathSize = L.sortOn (Down . view (#unSizedPath % _2)) . HSet.toList

-- | Retrieves the largest N paths.
--
-- @since 0.1
largestN :: Natural -> PathSizeData -> [SizedPath]
largestN n = L.take (fromIntegral n) . sortPathSize

-- | Displays the map.
--
-- @since 0.1
displayPathSize :: PathSizeData -> Text
displayPathSize = showList' . sortPathSize
  where
    showList' :: [SizedPath] -> Text
    showList' = TL.toStrict . TLB.toLazyText . foldl' go ""
    go acc (MkSizedPath (path, size)) =
      mconcat
        [ TLB.fromString $ show path,
          ": ",
          TLB.fromString $ show size,
          "\n",
          acc
        ]

-- | Gives the total size for a list of 'PathSizeData'.
--
-- @since 0.1
sumPathSizes :: [PathSizeData] -> Integer
sumPathSizes = foldl' (\a m -> a + sumPathSize m) 0

-- | Gives the total size for a 'PathSizeData'.
--
-- @since 0.1
sumPathSize :: PathSizeData -> Integer
sumPathSize = HSet.foldl' (\acc (MkSizedPath (_, x)) -> x + acc) 0
