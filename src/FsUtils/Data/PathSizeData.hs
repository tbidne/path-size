{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Supplies the 'PathSizeData' data type and related functions.
--
-- @since 0.1
module FsUtils.Data.PathSizeData
  ( Path (..),
    PathSizeData (..),
    SubPathSizeData (MkSubPathSizeData),
    sort,
    takeLargestN,
    display,
    sumSize,
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

unPath :: Path -> FilePath
unPath (Directory fp) = fp
unPath (File fp) = fp

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

-- | Structure representing the entire 'PathSizeData' tree corresponding
-- to a given path. That is, for a path @P@, 'SubPathSizeData' contains
-- a 'PathSizeData' for @P@ and _each of its sub-paths_.
--
-- @since 0.1
newtype SubPathSizeData = UnsafeSubPathSizeData (Seq PathSizeData)
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

-- | Pattern synonym for 'SubPathSizeData'. Note that construction
-- sorts the 'Seq'.
--
-- @since 0.1
pattern MkSubPathSizeData :: Seq PathSizeData -> SubPathSizeData
pattern MkSubPathSizeData xs <- UnsafeSubPathSizeData xs
  where
    MkSubPathSizeData xs = UnsafeSubPathSizeData (sort xs)

{-# COMPLETE MkSubPathSizeData #-}

unSubPathSizeData :: SubPathSizeData -> Seq PathSizeData
unSubPathSizeData (MkSubPathSizeData xs) = xs

-- | Sorts the path size.
--
-- @since 0.1
sort :: Seq PathSizeData -> Seq PathSizeData
sort = Seq.sortOn (Down . view (#unPathSizeData % _2))

-- | Retrieves the largest N paths.
--
-- @since 0.1
takeLargestN :: Natural -> Seq PathSizeData -> SubPathSizeData
takeLargestN n =
  UnsafeSubPathSizeData
    . Seq.take (fromIntegral n)
    . sort

-- | Displays the data.
--
-- @since 0.1
display :: SubPathSizeData -> Text
display = showList' . unSubPathSizeData
  where
    showList' :: Seq PathSizeData -> Text
    showList' = TL.toStrict . TLB.toLazyText . foldr go ""
    go (MkPathSizeData (path, size)) acc =
      mconcat
        [ TLB.fromString $ unPath path,
          ": ",
          TLB.fromLazyText $ TL.fromStrict $ formatSize size,
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

-- | Gives the total size for a 'Seq' 'PathSizeData'.
--
-- @since 0.1
sumSize :: Seq PathSizeData -> Natural
sumSize = foldl' (\acc (MkPathSizeData (_, x)) -> x + acc) 0
