{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'PathData' type.
--
-- @since 0.1
module PathSize.Data.PathData
  ( PathData (..),
    natify,
  )
where

import Control.DeepSeq (NFData)
import Effects.FileSystem.Utils (OsPath)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Optics.Core (A_Lens, LabelOptic (labelOptic), lensVL)

-- | Associates a path to its total (recursive) size in the file-system.
-- Takes a parameter for the size, allowing us to use a more efficient
-- temporary type (e.g. 'Integer') before finally converting to
-- Natural.
--
-- @since 0.1
data PathData a = MkPathData
  { -- | Path.
    --
    -- @since 0.1
    path :: !OsPath,
    -- | Size in bytes.
    --
    -- @since 0.1
    size :: !a,
    -- | Number of files.
    --
    -- @since 0.1
    numFiles :: !a,
    -- | Number of directories.
    --
    -- @since 0.1
    numDirectories :: !a
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
instance
  (k ~ A_Lens, a ~ OsPath, b ~ OsPath) =>
  LabelOptic "path" k (PathData s) (PathData s) a b
  where
  labelOptic = lensVL $ \f (MkPathData _path _size _numFiles _numDirectories) ->
    fmap (\path' -> MkPathData path' _size _numFiles _numDirectories) (f _path)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ s, b ~ s) =>
  LabelOptic "size" k (PathData s) (PathData s) a b
  where
  labelOptic = lensVL $ \f (MkPathData _path _size _numFiles _numDirectories) ->
    fmap (\size' -> MkPathData _path size' _numFiles _numDirectories) (f _size)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ s, b ~ s) =>
  LabelOptic "numFiles" k (PathData s) (PathData s) a b
  where
  labelOptic = lensVL $ \f (MkPathData _path _size _numFiles _numDirectories) ->
    fmap (\numFiles' -> MkPathData _path _size numFiles' _numDirectories) (f _numFiles)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ s, b ~ s) =>
  LabelOptic "numDirectories" k (PathData s) (PathData s) a b
  where
  labelOptic = lensVL $ \f (MkPathData _path _size _numFiles _numDirectories) ->
    fmap (MkPathData _path _size _numFiles) (f _numDirectories)
  {-# INLINE labelOptic #-}

-- | @since 0.1
natify :: PathData Integer -> PathData Natural
natify (MkPathData p s nf nd) =
  MkPathData p (fromIntegral s) (fromIntegral nf) (fromIntegral nd)
