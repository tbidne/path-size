{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'PathData' type.
--
-- @since 0.1
module PathSize.Data.PathData
  ( PathData (..),
  )
where

import Control.DeepSeq (NFData)
import Effects.FileSystem.Path (Path)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Optics.TH (makeFieldLabelsNoPrefix)

-- | Associates a path to its total (recursive) size in the file-system.
--
-- @since 0.1
data PathData = MkPathData
  { -- | Path.
    --
    -- @since 0.1
    path :: !Path,
    -- | Size in bytes.
    --
    -- @since 0.1
    size :: !Natural,
    -- | Number of files.
    --
    -- @since 0.1
    numFiles :: !Natural,
    -- | Number of directories.
    --
    -- @since 0.1
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
makeFieldLabelsNoPrefix ''PathData
