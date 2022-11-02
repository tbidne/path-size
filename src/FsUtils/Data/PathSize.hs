{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Supplies the 'PathSize' data type and related functions.
--
-- @since 0.1
module FsUtils.Data.PathSize
  ( Path (..),
    SizedPath (..),
    sortPathSize,
    largestN,
    displayPathSize,
    sumPathSize,
  )
where

import Control.DeepSeq (NFData)
import Data.Foldable (Foldable (foldl'))
import Optics.TH (makeFieldLabelsNoPrefix)
import Optics.Core (view, _2, (%))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
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


-- TODO:
-- Try unboxed types

-- | Sorts the path size.
--
-- @since 0.1
sortPathSize :: Seq SizedPath -> Seq SizedPath
sortPathSize = Seq.sortOn (Down . view (#unSizedPath % _2))

-- | Retrieves the largest N paths.
--
-- @since 0.1
largestN :: Natural -> Seq SizedPath -> Seq SizedPath
largestN n = Seq.take (fromIntegral n) . sortPathSize

-- | Displays the map.
--
-- @since 0.1
displayPathSize :: Seq SizedPath -> Text
displayPathSize = showList' . sortPathSize
  where
    showList' :: Seq SizedPath -> Text
    showList' = TL.toStrict . TLB.toLazyText . foldl' go ""
    go acc (MkSizedPath (path, size)) =
      mconcat
        [ TLB.fromString $ show path,
          ": ",
          TLB.fromString $ show size,
          "\n",
          acc
        ]

-- | Gives the total size for a 'PathSizeData'.
--
-- @since 0.1
sumPathSize :: Seq SizedPath -> Integer
sumPathSize = foldl' (\acc (MkSizedPath (_, x)) -> x + acc) 0
