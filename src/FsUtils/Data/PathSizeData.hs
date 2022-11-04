{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Supplies the 'PathSizeData' data type and related functions.
--
-- @since 0.1
module FsUtils.Data.PathSizeData
  ( Path (..),
    PathSizeData (..),
    sort,
    takeLargestN,
    display,
    sumSize,
  )
where

import Control.DeepSeq (NFData)
import Data.Foldable (Foldable (foldl'))
import Data.Ord (Down (Down))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Optics.Core (view, (%), _2)
import Optics.TH (makeFieldLabelsNoPrefix)

-- | Path types.

--- @since 0.1
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

-- | Associates a 'Path' to its total (recursive) size in the file-system.
--
-- @since 0.1
newtype PathSizeData = MkPathSizeData
  { unPathSizeData :: (Path, Natural)
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
makeFieldLabelsNoPrefix ''PathSizeData

-- TODO:
-- Try unboxed types

-- | Sorts the path size.
--
-- @since 0.1
sort :: Seq PathSizeData -> Seq PathSizeData
sort = Seq.sortOn (Down . view (#unPathSizeData % _2))

-- | Retrieves the largest N paths.
--
-- @since 0.1
takeLargestN :: Natural -> Seq PathSizeData -> Seq PathSizeData
takeLargestN n = Seq.take (fromIntegral n) . sort

-- | Displays the data.
--
-- @since 0.1
display :: Seq PathSizeData -> Text
display = showList' . sort
  where
    showList' :: Seq PathSizeData -> Text
    showList' = TL.toStrict . TLB.toLazyText . foldl' go ""
    go acc (MkPathSizeData (path, size)) =
      mconcat
        [ TLB.fromString $ show path,
          ": ",
          TLB.fromString $ show size,
          "\n",
          acc
        ]

-- | Gives the total size for a 'Seq' 'PathSizeData'.
--
-- @since 0.1
sumSize :: Seq PathSizeData -> Natural
sumSize = foldl' (\acc (MkPathSizeData (_, x)) -> x + acc) 0
