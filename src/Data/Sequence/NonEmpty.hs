-- | @since 0.1
module Data.Sequence.NonEmpty
  ( NonEmptySeq (.., (:||)),
    unNonEmptySeq,
  )
where

import Control.DeepSeq (NFData)
import Data.Sequence (Seq, (<|))
import GHC.Generics (Generic)

-- TODO: IS it better to have a newtype tuple (possible unboxed?) or data?

-- | Non-empty 'Seq'.
--
-- @since 0.1
newtype NonEmptySeq a = MkNonEmptySeq (a, Seq a)
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
instance Foldable NonEmptySeq where
  foldr f acc (x :|| xs) = foldr f acc (x <| xs)

-- | Pattern synonym for 'NonEmptySeq'.
--
-- @since 0.1
pattern (:||) :: a -> Seq a -> NonEmptySeq a
pattern pd :|| xs <- MkNonEmptySeq (pd, xs)
  where
    pd :|| xs = MkNonEmptySeq (pd, xs)

{-# COMPLETE (:||) #-}

-- | @since 0.1
unNonEmptySeq :: NonEmptySeq a -> Seq a
unNonEmptySeq (x :|| xs) = x <| xs
