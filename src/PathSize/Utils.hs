{-# LANGUAGE CPP #-}

-- | Provides utilities.
--
-- @since 0.1
module PathSize.Utils
  ( -- * Windows / Unix compat
    PosixC,
    runPosixC,

    -- * Functions
    hidden,
    getFileStatus,

    -- * Misc
    unzipResultSeq,
  )
where

#if !MIN_VERSION_base(4, 20, 0)
import Data.Foldable (foldl')
#endif
import Data.Kind (Type)
import Data.Sequence (Seq (Empty, (:<|)))
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Effectful (Eff, IOE, (:>))
import FileSystem.OsPath (OsPath)
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
import Effectful.Posix.Dynamic (Posix, runPosix)
import Effectful.Posix.Dynamic qualified as Posix
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
import Effectful.PosixCompat.Dynamic (PosixCompat, runPosixCompat)
import Effectful.PosixCompat.Dynamic qualified as PosixCompat
import FileSystem.OsPath qualified as FS.OsPath
import GHC.Stack (HasCallStack)
#endif

-- We might expect PosixCompat.Static to be faster than PosixCompat.Dynamic,
-- since it is used in a tight loop, but benchmarks do not show an
-- improvement. Since we currently rely on dynamic dispatch in our
-- function tests, we keep therefore keep the Dynamic effect.
--
-- See NOTE: [Functional test errors].

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

-- | Handles posix effect.
runPosixC :: (IOE :> es) => Eff (PosixC : es) a -> Eff es a

type PosixC :: (Type -> Type) -> Type -> Type

#if POSIX
hidden p = case Short.uncons2 sbs of
  Nothing -> False
  Just (46, 47, _) -> False -- "./"
  Just (46, _, _) -> True   -- "."
  Just _ -> False
  where
    sbs = p.getOsString.getPosixString

-- | Alias for Posix* constraints. On Posix, this is Posix (unix),
-- which allows for greater efficiency. On Windows, this is just
-- PosixCompat (unix-compat).
type PosixC = Posix

runPosixC = runPosix

#else

hidden = const False

-- | Alias for Posix* constraints. On Posix, this is Posix (unix),
-- which allows for greater efficiency. On Windows, this is just
-- PosixCompat (unix-compat).
type PosixC = PosixCompat

runPosixC = runPosixCompat

#endif

#if POSIX
-- | Retrieves the FileStatus for the given path.
--
-- @since 0.1
getFileStatus ::
  forall es.
  ( PosixC :> es
  ) =>
  OsPath ->
  Eff es FileStatus
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
  forall es.
  ( HasCallStack,
    PosixC :> es
  ) =>
  OsPath ->
  Eff es FileStatus
getFileStatus path = do
  -- It would be nice if we could do something similar here i.e. take advantage
  -- of the fact that we know OsPath is a WindowsString and call the relevant
  -- function directly. Alas, the getSymbolicLinkStatus logic in PosixCompat
  -- is bespoke, and there does not appear to be a drop-in replacement
  -- @getSymbolicLinkStatus :: WindowsString -> IO FileStatus@ in Wind32.
  fp <- FS.OsPath.decodeThrowM path
  PosixCompat.getSymbolicLinkStatus fp
#endif
