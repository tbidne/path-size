{-# LANGUAGE CPP #-}

-- | Provides utilities.
--
-- @since 0.1
module PathSize.Utils
  ( -- * Windows / Unix compat
    MonadPosixC,
    hidden,
    getFileStatus,

    -- * Misc
    unzipResultSeq,
  )
where

#if !MIN_VERSION_base(4, 20, 0)
import Data.Foldable (foldl')
#endif
import Data.Sequence (Seq (Empty, (:<|)))
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Effects.FileSystem.OsPath (OsPath)
import PathSize.Data.PathSizeResult
  ( PathSizeResult
      ( PathSizeFailure,
        PathSizePartial,
        PathSizeSuccess
      ),
  )
import PathSize.Data.PathTree (PathTree)
import PathSize.Exception (PathE)
import System.PosixCompat.Files (FileStatus)

#if POSIX
import Effects.Exception (HasCallStack)
import Effects.System.Posix (MonadPosix)
import Effects.System.Posix qualified as Posix
import System.OsString.Internal.Types
  ( OsString (getOsString),
    PosixString(getPosixString),
  )
#if OS_STRING
import System.OsString.Data.ByteString.Short qualified as Short
#else
import System.OsPath.Data.ByteString.Short qualified as Short
#endif
#else
import Effects.Exception (HasCallStack, MonadThrow)
import Effects.FileSystem.OsPath qualified as FS.OsPath
import Effects.System.PosixCompat (MonadPosixCompat)
import Effects.System.PosixCompat qualified as PosixCompat
#endif

-- | Unzips a sequence of results.
--
-- @since 0.1
unzipResultSeq :: Seq (PathSizeResult PathTree) -> (Seq PathE, Seq PathTree)
unzipResultSeq = foldl' f (Empty, Empty)
  where
    f (errs, trees) = \case
      PathSizeSuccess tree -> (errs, tree :<| trees)
      PathSizePartial (e :<|| es) tree -> (e :<| es <> errs, tree :<| trees)
      PathSizeFailure (e :<|| es) -> (e :<| es <> errs, trees)

-- | Detects hidden paths via a rather crude 'dot' check, with an
-- exception for the current directory ./.
--
-- @since 0.1
hidden :: OsPath -> Bool
#if POSIX
hidden p = case Short.uncons2 sbs of
  Nothing -> False
  Just (46, 47, _) -> False -- "./"
  Just (46, _, _) -> True   -- "."
  Just _ -> False
  where
    sbs = p.getOsString.getPosixString
#else
hidden = const False
#endif

{- ORMOLU_DISABLE -}

-- | Alias for MonadPosix* constraints. On Posix, this is MonadPosix (unix),
-- which allows for greater efficiency. On Windows, this is just
-- MonadPosixCompat (unix-compat).
type MonadPosixC m =
#if POSIX
  MonadPosix m
#else
  MonadPosixCompat m
#endif

{- ORMOLU_ENABLE -}

#if POSIX
-- | Retrieves the FileStatus for the given path.
--
-- @since 0.1
getFileStatus ::
  forall m.
  ( HasCallStack,
    MonadPosixC m
  ) =>
  OsPath ->
  m FileStatus
getFileStatus path =
  -- NOTE: On posix, we can take advantage of the fact that we know OsPath
  -- is a PosixString. This means we can call the unix library directly,
  -- saving expensive @OsPath -> FilePath (unix-compat)@ and
  -- @FilePath -> PosixString (unix)@ conversions.
  --
  -- Because we are getting the FileStatus directly, we also have more
  -- freedom to change the underlying size type for more potential
  -- efficiency gains, as we have one fewer fromIntegral call. The normal
  -- getFileSize already converts the stats to Integer, which is wasted if
  -- that's not the type we want.
  Posix.getSymbolicLinkStatus path.getOsString
#else
-- | Retrieves the FileStatus for the given path.
--
-- @since 0.1
getFileStatus ::
  forall m.
  ( HasCallStack,
    MonadPosixC m,
    MonadThrow m
  ) =>
  OsPath ->
  m FileStatus
getFileStatus path = do
  -- It would be nice if we could do something similar here i.e. take advantage
  -- of the fact that we know OsPath is a WindowsString and call the relevant
  -- function directly. Alas, the getSymbolicLinkStatus logic in PosixCompat
  -- is bespoke, and there does not appear to be a drop-in replacement
  -- @getSymbolicLinkStatus :: WindowsString -> IO FileStatus@ in Wind32.
  fp <- FS.OsPath.decodeThrowM path
  PosixCompat.getSymbolicLinkStatus fp
#endif
