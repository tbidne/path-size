{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Supplies the 'PathSizeData' data type and related functions.
--
-- @since 0.1
module FsSize.Data.PathSizeData
  ( -- * Base types
    Path (..),
    PathSizeData (..),

    -- * Aggregate paths
    PathTree (..),
    takeLargestN,
    SubPathSizeData,
    mkSubPathSizeData,
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
import Data.Ord (Down (Down))
import Data.Sequence (Seq, (<|))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Optics.Core (view)
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
data PathSizeData = MkPathSizeData
  { path :: !Path,
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
makeFieldLabelsNoPrefix ''PathSizeData

-- | Given a path, represents the directory tree, with each subpath
-- associated to its size. This structure is essentially a rose tree.
--
-- @since 0.1
data PathTree
  = Nil
  | Node !PathSizeData !(Seq PathTree)
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

toSeq :: PathTree -> Seq PathSizeData
toSeq (Node x subTrees) = x <| (subTrees >>= toSeq)
toSeq Nil = []

-- | A flattened and sorted representation of 'PathTree'.
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

-- | Creates a 'SubPathSizeData' from a 'PathTree'.
--
-- @since 0.1
mkSubPathSizeData :: PathTree -> SubPathSizeData
mkSubPathSizeData = UnsafeSubPathSizeData . sort . toSeq

unSubPathSizeData :: SubPathSizeData -> Seq PathSizeData
unSubPathSizeData (UnsafeSubPathSizeData xs) = xs

-- | Sorts the path size.
--
-- @since 0.1
sort :: Seq PathSizeData -> Seq PathSizeData
sort = Seq.sortOn (Down . view #size)

-- | Retrieves the largest N paths.
--
-- @since 0.1
takeLargestN :: Natural -> PathTree -> SubPathSizeData
takeLargestN n =
  UnsafeSubPathSizeData
    . Seq.take (fromIntegral n)
    . sort
    . toSeq

-- | Displays the data.
--
-- @since 0.1
display :: SubPathSizeData -> Text
display = showList' . unSubPathSizeData
  where
    showList' :: Seq PathSizeData -> Text
    showList' = TL.toStrict . TLB.toLazyText . foldr go ""
    go (MkPathSizeData {path, size, numFiles, numDirectories}) acc =
      mconcat
        [ TLB.fromString $ unPath path,
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
