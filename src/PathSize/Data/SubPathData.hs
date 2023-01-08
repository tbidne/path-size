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
    display,

    -- * Reexports
    NESeq,
  )
where

import Data.Sequence.NonEmpty (NESeq)
import PathSize.Data.SubPathData.Internal
