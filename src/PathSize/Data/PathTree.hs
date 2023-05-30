-- | Provides 'PathTree' type.
--
-- @since 0.1
module PathSize.Data.PathTree
  ( PathTree (..),
    singleton,
    pathTreeToSeq,
  )
where

import Control.DeepSeq (NFData)
import Data.Sequence (Seq (Empty, (:<|)))
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import GHC.Generics (Generic)
import PathSize.Data.PathData (PathData)

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

-- | Flattens a 'PathTree' into a 'Seq'.
--
-- @since 0.1
pathTreeToSeq :: PathTree -> NESeq (PathData Integer)
pathTreeToSeq (x :^| subTrees) = x :<|| (subTrees >>= pathTreeToSeq')
  where
    pathTreeToSeq' :: PathTree -> Seq (PathData Integer)
    pathTreeToSeq' (x' :^| subTrees') = x' :<| (subTrees' >>= pathTreeToSeq')

-- | @since 0.1
singleton :: PathData Integer -> PathTree
singleton pd = pd :^| Empty
