{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- NOTE: For reasons I don't understand, for GHC < 9.4, we receive an error:
--
--  • Ignoring unusable UNPACK pragma
--      on the second argument of ‘MkPathData’
--  • In the definition of data constructor ‘MkPathData’
--    In the data type declaration for ‘PathData’
--
-- for the unpacked Integer fields. Really it's just a warning, but it doesn't
-- appear to have a custom warning, and is triggered by Werror on CI.
--
-- We __don't__ receive this warning for higher GHCs, so presumably it does
-- what we want? Hence I don't want to remove the unpacks. It appears there is
-- no way to silence this specific error (...sigh), so we are left with
-- disabling Werror.

#if !MIN_VERSION_base(4,18,0)
{-# OPTIONS_GHC -Wwarn #-}
#endif

-- | Provides 'PathData' type.
--
-- @since 0.1
module PathSize.Data.PathData
  ( PathData (..),
  )
where

import Control.DeepSeq (NFData)
import Effects.FileSystem.OsPath (OsPath)
import GHC.Generics (Generic)
import Optics.Core (A_Lens, LabelOptic (labelOptic), lensVL)

-- NOTE: [Efficient Int Type]
--
-- Currently, sizes use the type 'Integer'. Due to the potentialy for a very
-- large size (e.g. a gigabytes => bilions), the only appropriate types are
-- unbounded (Integer, Natural) or 64 bits (Int64, Word64).
--
-- One would think Int64 would be the fastest, since:
--
--   1. Fixed size. Unbounded require branching.
--   2. The underlying size function we use, unix's getFileSize returns a
--      newtype wrapper over Int64, so theoretically this should be free. Any
--      other type requires a fromIntegral conversion in a very tight loop.
--
-- Confusingly, Integer seems to be the fastest. It's possible that this is
-- a benchmark problem, but we cannot justify a switch when the benchmarks
-- get __worse__. Keep this in mind, maybe we can find an improvement in the
-- future.
--
-- It might be worth exploring, say, Int64#, but the difficulty there is that
-- we'd probably have to reimplement much of the sorting outselves, since
-- all the built-in sort logic assumes Type.

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
