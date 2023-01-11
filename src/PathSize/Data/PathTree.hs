{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'PathTree' type.
--
-- @since 0.1
module PathSize.Data.PathTree
  ( PathTree (..),
    pathTreeToSeq,
    _Nil,
    _Node,
  )
where

import Control.DeepSeq (NFData)
import Data.Sequence (Seq, (<|))
import GHC.Generics (Generic)
import Optics.TH (makePrisms)
import PathSize.Data.PathData (PathData)

-- | Given a path, represents the directory tree, with each subpath
-- associated to its size. This structure is essentially a rose tree.
--
-- @since 0.1
data PathTree
  = Nil
  | Node !(PathData Integer) !(Seq PathTree)
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

-- | Flattens a 'PathTree' into a 'Seq'.
--
-- @since 0.1
pathTreeToSeq :: PathTree -> Seq (PathData Integer)
pathTreeToSeq (Node x subTrees) = x <| (subTrees >>= pathTreeToSeq)
pathTreeToSeq Nil = []

-- | @since 0.1
makePrisms ''PathTree
