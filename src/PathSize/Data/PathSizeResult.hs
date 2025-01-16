{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'PathSizeResult' type.
--
-- @since 0.1
module PathSize.Data.PathSizeResult
  ( PathSizeResult (..),
    mkPathE,
    mkPathEString,
    _PathSizeSuccess,
    _PathSizePartial,
    _PathSizeFailure,
  )
where

import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Control.Exception qualified as E
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import FileSystem.OsPath (OsPath)
import GHC.Generics (Generic)
import Optics.Core (Prism', prism)
import PathSize.Exception (PathE (MkPathE))

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

-- | @since 0.1
mkPathE :: (Exception e) => OsPath -> e -> PathSizeResult a

-- NOTE: For base 4.20 (GHC 9.10), there is a callstack on the SomeException
-- itself. We don't really want this as it clutters the output (and fails
-- a functional test). So in this case we walk the SomeException to avoid
-- the callstack.
--
-- In later base versions, the callstack is separate, so we have no problems.
#if MIN_VERSION_base(4, 20, 0) && !MIN_VERSION_base(4, 21, 0)
mkPathE path ex = case E.toException ex of
  E.SomeException e -> mkPathEString path . E.displayException $ e
#else
mkPathE path = mkPathEString path . E.displayException
#endif

mkPathEString :: OsPath -> String -> PathSizeResult a
mkPathEString path = PathSizeFailure . NESeq.singleton . MkPathE path
