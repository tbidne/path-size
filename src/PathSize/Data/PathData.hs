{-# LANGUAGE TemplateHaskell #-}
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
import Effects.FileSystem.Path (Path)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Optics.TH (makeFieldLabelsNoPrefix)

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
    path :: !Path,
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
makeFieldLabelsNoPrefix ''PathData

-- | @since 0.1
natify :: PathData Integer -> PathData Natural
natify (MkPathData p s nf nd) =
  MkPathData p (fromIntegral s) (fromIntegral nf) (fromIntegral nd)
