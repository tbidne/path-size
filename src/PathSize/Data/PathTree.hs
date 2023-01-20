{-# LANGUAGE OverloadedLists #-}

-- | Provides 'PathTree' type.
--
-- @since 0.1
module PathSize.Data.PathTree
  ( PathTree (..),
    emptyPathTree,
    pathTreeToSeq,
  )
where

import Control.DeepSeq (NFData)
import Data.Sequence (Seq ((:<|)))
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Effects.FileSystem.Path (Path)
import GHC.Generics (Generic)
import PathSize.Data.PathData (PathData, emptyPathData)

-- | Given a path, represents the directory tree, with each subpath
-- associated to its size. This structure is essentially a rose tree.
--
-- @since 0.1
data PathTree = !(PathData Integer) :^| !(Seq PathTree)
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
emptyPathTree :: Path -> PathTree
emptyPathTree p = emptyPathData p :^| []
{-# INLINEABLE emptyPathTree #-}

-- | Flattens a 'PathTree' into a 'Seq'.
--
-- @since 0.1
pathTreeToSeq :: PathTree -> NESeq (PathData Integer)
pathTreeToSeq (x :^| subTrees) = x :<|| (subTrees >>= pathTreeToSeq')
  where
    pathTreeToSeq' :: PathTree -> Seq (PathData Integer)
    pathTreeToSeq' (x' :^| subTrees') = x' :<| (subTrees' >>= pathTreeToSeq')
