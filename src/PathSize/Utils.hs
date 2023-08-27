-- | Provides utilities.
--
-- @since 0.1
module PathSize.Utils
  ( tryCalcSymLink,
    tryCalcFile,
    unzipResultSeq,
    hidden,
  )
where

import Control.Monad ((<=<))
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.Sequence (Seq (Empty, (:<|)))
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Effectful (Eff, (:>))
import Effectful.Exception (tryAny)
import Effectful.FileSystem.PathReader.Dynamic (PathReaderDynamic)
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.FileSystem.Utils (OsPath)
import Effectful.FileSystem.Utils qualified as FsUtils
import Effectful.PosixCompat.Static (PosixCompatStatic)
import Effectful.PosixCompat.Static qualified as Posix
import PathSize.Data.PathData
  ( PathData
      ( MkPathData,
        numDirectories,
        numFiles,
        path,
        size
      ),
  )
import PathSize.Data.PathSizeResult
  ( PathSizeResult
      ( PathSizeFailure,
        PathSizePartial,
        PathSizeSuccess
      ),
    mkPathE,
  )
import PathSize.Data.PathTree (PathTree)
import PathSize.Data.PathTree qualified as PathTree
import PathSize.Exception (PathE)
import System.PosixCompat.Files qualified as PFiles

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
hidden p = case FsUtils.decodeOsToFp p of
  Right ('.' : '/' : _) -> False
  Right ('.' : _) -> True
  _ -> False

tryCalcSymLink ::
  ( PosixCompatStatic :> es
  ) =>
  OsPath ->
  Eff es (PathSizeResult PathTree)
tryCalcSymLink =
  tryCalcSize
    (fmap fromIntegral . getSymLinkSize)
  where
    getSymLinkSize =
      fmap PFiles.fileSize
        . Posix.getSymbolicLinkStatus
        <=< FsUtils.decodeOsToFpThrowM
{-# INLINEABLE tryCalcSymLink #-}

tryCalcFile ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es (PathSizeResult PathTree)
tryCalcFile = tryCalcSize PR.getFileSize

tryCalcSize ::
  (OsPath -> Eff es Integer) ->
  OsPath ->
  Eff es (PathSizeResult PathTree)
tryCalcSize sizeFn path = do
  tryAny (sizeFn path) <&> \case
    Left ex -> mkPathE path ex
    Right size ->
      PathSizeSuccess $
        PathTree.singleton $
          MkPathData
            { path,
              size,
              numFiles = 1,
              numDirectories = 0
            }
{-# INLINEABLE tryCalcSize #-}
