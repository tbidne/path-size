{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'PathData' type.
--
-- @since 0.1
module PathSize.Data.PathData
  ( PathData (..),
  )
where

import Control.DeepSeq (NFData)
import Effects.FileSystem.Utils (OsPath)
import GHC.Generics (Generic)
import Optics.Core (A_Lens, LabelOptic (labelOptic), lensVL)

-- | Associates a path to its total (recursive) size in the file-system.
-- Takes a parameter for the size, allowing us to use a more efficient
-- temporary type (e.g. 'Integer') before finally converting to
-- Natural.
--
-- @since 0.1
data PathData = MkPathData
  { -- | Path.
    --
    -- @since 0.1
    path :: !OsPath,
    -- | Size in bytes.
    --
    -- @since 0.1
    size :: {-# UNPACK #-} !Integer,
    -- | Number of files.
    --
    -- @since 0.1
    numFiles :: {-# UNPACK #-} !Integer,
    -- | Number of directories.
    --
    -- @since 0.1
    numDirectories :: {-# UNPACK #-} !Integer
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
  LabelOptic "path" k PathData PathData a b
  where
  labelOptic = lensVL $ \f (MkPathData _path _size _numFiles _numDirectories) ->
    fmap (\path' -> MkPathData path' _size _numFiles _numDirectories) (f _path)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Integer, b ~ Integer) =>
  LabelOptic "size" k PathData PathData a b
  where
  labelOptic = lensVL $ \f (MkPathData _path _size _numFiles _numDirectories) ->
    fmap (\size' -> MkPathData _path size' _numFiles _numDirectories) (f _size)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Integer, b ~ Integer) =>
  LabelOptic "numFiles" k PathData PathData a b
  where
  labelOptic = lensVL $ \f (MkPathData _path _size _numFiles _numDirectories) ->
    fmap (\numFiles' -> MkPathData _path _size numFiles' _numDirectories) (f _numFiles)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Integer, b ~ Integer) =>
  LabelOptic "numDirectories" k PathData PathData a b
  where
  labelOptic = lensVL $ \f (MkPathData _path _size _numFiles _numDirectories) ->
    fmap (MkPathData _path _size _numFiles) (f _numDirectories)
  {-# INLINE labelOptic #-}
