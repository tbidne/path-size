{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'PathSizeResult' type.
--
-- @since 0.1
module PathSize.Data.PathSizeResult
  ( PathSizeResult (..),
    _PathSizeSuccess,
    _PathSizePartial,
    _PathSizeFailure,
  )
where

import Control.DeepSeq (NFData)
import Data.Sequence.NonEmpty (NESeq)
import GHC.Generics (Generic)
import Optics.Core (Prism', prism)
import PathSize.Exception (PathE)

-- | Result of running a path-size computation with multiple notions of
-- failure.
--
-- @since 0.1
data PathSizeResult a
  = -- | Successfully computed @a@.
    --
    -- @since 0.1
    PathSizeSuccess !a
  | -- | Computed @a@ with some errors.
    --
    -- @since 0.1
    PathSizePartial !(NESeq PathE) !a
  | -- | Failed computing @a@.
    --
    -- @since 0.1
    PathSizeFailure !(NESeq PathE)
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Functor,
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
_PathSizeSuccess :: Prism' (PathSizeResult a) a
_PathSizeSuccess =
  prism
    PathSizeSuccess
    ( \x -> case x of
        PathSizeSuccess a -> Right a
        _ -> Left x
    )
{-# INLINE _PathSizeSuccess #-}

-- | @since 0.1
_PathSizePartial :: Prism' (PathSizeResult a) (NESeq PathE, a)
_PathSizePartial =
  prism
    (uncurry PathSizePartial)
    ( \x -> case x of
        PathSizePartial errs a -> Right (errs, a)
        _ -> Left x
    )
{-# INLINE _PathSizePartial #-}

-- | @since 0.1
_PathSizeFailure :: Prism' (PathSizeResult a) (NESeq PathE)
_PathSizeFailure =
  prism
    PathSizeFailure
    ( \x -> case x of
        PathSizeFailure errs -> Right errs
        _ -> Left x
    )
{-# INLINE _PathSizeFailure #-}
