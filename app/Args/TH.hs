{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Args.TH
  ( gitData,
  )
where

import Control.Applicative (liftA3)
import Control.Exception (Exception (displayException))
import Data.Bifunctor (Bifunctor (first))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Clock.POSIX qualified as PosixTime
import Data.Time.Format qualified as Fmt
import Development.GitRev.Typed.OsString
  ( EnvError (MkEnvError),
    GitError,
    GitRevError (GitRevErrorEnv, GitRevErrorText),
    IndexUsed (IdxNotUsed),
  )
import Development.GitRev.Typed.OsString qualified as GRT
import FileSystem.OsString qualified as FS.OsString
import Language.Haskell.TH (Code, Q)
import System.OsString (OsString, osstr)
import System.OsString qualified as OsString
import Text.Read qualified as TR

gitData :: Code Q (OsString, OsString, OsString)
gitData = toCode qs
  where
    toCode = GRT.qToCode . GRT.projectError

    qs =
      GRT.firstSuccessQ
        [ GRT.embedGitError gitDataFromGitQ,
          GRT.runGitInEnvDirQ [osstr|PATH_SIZE_HOME|] gitDataFromGitQ,
          gitDataFromEnvQ
        ]

-- | Normal process, get info from git.
gitDataFromGitQ :: Q (Either GitError (OsString, OsString, OsString))
gitDataFromGitQ = do
  -- We use custom runGit rather than normal gitCommitDateQ because the
  -- latter uses --format=%cd e.g.
  --
  --     Thu May 1 14:05:35 2025 +1200
  --
  -- whereas we want --format=%cs i.e.
  --
  --     2025-05-01
  --
  -- We do this because we want consistency with nix, and unfortunately
  -- nix only gives us a local timestamp without any timezone information.
  -- So we throw away the timezone here too. Notice there is still a
  -- possibility of discrepancies because runGit implicitly includes zone
  -- info, though we live with it, since it's a minor issue.
  d <-
    GRT.runGitQ
      [[osstr|log|], [osstr|HEAD|], [osstr|-1|], [osstr|--format=%cs|]]
      IdxNotUsed
  h <- GRT.gitHashQ
  sh <- GRT.gitShortHashQ
  pure $ liftA3 (,,) d h sh

-- | Backup for when we cannot use git e.g. nix. We instead get the data
-- from environment variables:
--
-- - PATH_SIZE_MODIFIED: unix time like "1746055623"
-- - PATH_SIZE_HASH: long hash
-- - PATH_SIZE_SHORT_HASH: short hash
--
-- We have to convert the unix time into the intended format
-- YYYY-MM-DD.
gitDataFromEnvQ :: Q (Either GitRevError (OsString, OsString, OsString))
gitDataFromEnvQ = do
  let dateVar = [osstr|PATH_SIZE_MODIFIED|]
  d <- GRT.embedEnvError $ fmap (>>= displayUnixTime dateVar) (GRT.envValQ dateVar)
  h <- validateHash 40 <$> GRT.envValQ [osstr|PATH_SIZE_HASH|]
  sh <- validateHash 7 <$> GRT.envValQ [osstr|PATH_SIZE_SHORT_HASH|]
  pure $ liftA3 (,,) d h sh
  where
    validateHash :: Int -> Either EnvError OsString -> Either GitRevError OsString
    validateHash n = joinErrors . fmap (validateHash' n)

    joinErrors = GRT.joinFirst GitRevErrorEnv GitRevErrorText

    validateHash' :: Int -> OsString -> Either Text OsString
    validateHash' n str
      | strLen /= n && strLen /= dirtyLen =
          Left $
            mconcat
              [ "Expected hash length ",
                showt n,
                " or ",
                showt dirtyLen,
                ", received ",
                showt strLen,
                ": ",
                T.pack (FS.OsString.decodeLenient str)
              ]
      | hasInvalidChar str =
          Left $
            "Invalid char in hash: "
              <> T.pack (FS.OsString.decodeLenient str)
      | otherwise = Right str
      where
        strLen = OsString.length str
        dirtyLen = n + 6

    hasInvalidChar :: OsString -> Bool
    hasInvalidChar str =
      -- We allow dirty hashes to have invalid chars, due to the '-dirty'
      -- suffix.
      let hasNonHexChar =
            OsString.any
              (not . (\c -> OsString.elem c [osstr|0123456789abcdefABCDEF|]))
              str
          isDirty = [osstr|-dirty|] `OsString.isSuffixOf` str
       in hasNonHexChar && not isDirty

displayUnixTime :: OsString -> OsString -> Either EnvError OsString
displayUnixTime var unixTimeOsStr = do
  unixTimeStr <-
    first (mapEx "Error decoding to String: ") $
      FS.OsString.decode unixTimeOsStr

  unixSeconds <-
    first (\s -> mapEnvError $ "Error reading seconds: " ++ s) $
      TR.readEither @Integer unixTimeStr

  let posixTime = fromInteger @POSIXTime unixSeconds
      utcTime = PosixTime.posixSecondsToUTCTime posixTime
      utcFormatted = Fmt.formatTime Fmt.defaultTimeLocale "%Y-%m-%d" utcTime

  first (mapEx "Error endecoding to OsString: ") $
    FS.OsString.encode utcFormatted
  where
    mapEx :: forall e. (Exception e) => String -> e -> EnvError
    mapEx s = mapEnvError . (s ++) . displayException

    mapEnvError :: String -> EnvError
    mapEnvError str =
      MkEnvError
        { var,
          value = Just unixTimeOsStr,
          reason = FS.OsString.encodeLenient str
        }

showt :: (Show a) => a -> Text
showt = T.pack . show
