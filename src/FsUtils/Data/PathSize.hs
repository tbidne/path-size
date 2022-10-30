-- | Supplies the 'PathSize' data type and related functions.
--
-- @since 0.1
module FsUtils.Data.PathSize
  ( Path (..),
    PathSize,
    largestN,
    displayPathSize,
    sumPathSizes,
    sumPathSize,
  )
where

import Control.DeepSeq (NFData)
import Data.Foldable (Foldable (foldl'))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HMap
import Data.Hashable (Hashable)
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

-- TODO: Should this be a regular Map, using its Ord rather than sorting
-- the HashMap later? Benchmark.
--
-- Also, if we end up using a sorted map, we should consider using a Set
-- since we want to sort on the size, and at that point there's no reason
-- to prefer the map (Path -> Integer) over a set (Path, Integer)

-- | Associates paths to their sizes.
--
-- @since 0.1
type PathSize = HashMap Path Integer

-- | Retrieves the largest N paths.
--
-- @since 0.1
largestN :: Natural -> PathSize -> [(Path, Integer)]
largestN n = takeN
  where
    takeN =
      L.take (fromIntegral n)
        . L.sortOn (Down . snd)
        . HMap.toList

-- | Displays the map.
--
-- @since 0.1
displayPathSize :: PathSize -> Text
displayPathSize = showList' . L.sortOn (Down . snd) . HMap.toList
  where
    showList' :: [(Path, Integer)] -> Text
    showList' = TL.toStrict . TLB.toLazyText . foldl' go ""
    go acc (path, size) =
      mconcat
        [ TLB.fromString $ show path,
          ": ",
          TLB.fromString $ show size,
          "\n",
          acc
        ]

-- | Gives the total size for a list of 'PathSize'.
--
-- @since 0.1
sumPathSizes :: [PathSize] -> Integer
sumPathSizes = foldl' (\a m -> a + sumPathSize m) 0

-- | Gives the total size for a 'PathSize'.
--
-- @since 0.1
sumPathSize :: PathSize -> Integer
sumPathSize = HMap.foldl' (+) 0
