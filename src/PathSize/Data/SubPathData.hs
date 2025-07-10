-- | Provides 'SubPathData' type.
--
-- @since 0.1
module PathSize.Data.SubPathData
  ( -- * Type
    SubPathData (MkSubPathData),

    -- * Construction
    mkSubPathData,
    takeLargestN,

    -- * Elimination
    unSubPathData,

    -- * Operations
    DisplayConfig (..),
    defaultDisplayConfig,
    DisplayFormat (..),
    display,

    -- * Optics
    _DisplayFormatSingle,
    _DisplayFormatTabular,

    -- * Reexports
    NESeq,
  )
where

import Data.Sequence.NonEmpty (NESeq)
import PathSize.Data.SubPathData.Internal
  ( DisplayConfig (MkDisplayConfig, color, format, reverseSort),
    DisplayFormat (DisplayFormatSingle, DisplayFormatTabular),
    SubPathData (MkSubPathData),
    defaultDisplayConfig,
    display,
    mkSubPathData,
    takeLargestN,
    unSubPathData,
    _DisplayFormatSingle,
    _DisplayFormatTabular,
  )
