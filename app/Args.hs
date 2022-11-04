{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides CLI args functionality.
--
-- @since 0.1
module Args
  ( getArgs,
    Args (..),
  )
where

import Data.List qualified as L
import Data.String (IsString (fromString))
import Data.Version.Package qualified as PV
import Development.GitRev qualified as GitRev
import FsUtils.Data.PathSizeConfig
  ( PathSizeConfig (MkPathSizeConfig),
    Strategy (..),
  )
import GHC.Natural (Natural)
import Optics.TH (makeFieldLabelsNoPrefix)
import Options.Applicative
  ( Parser,
    ParserInfo
      ( ParserInfo,
        infoFailureCode,
        infoFooter,
        infoFullDesc,
        infoHeader,
        infoParser,
        infoPolicy,
        infoProgDesc
      ),
    (<**>),
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help.Chunk (Chunk (Chunk))
import Options.Applicative.Types (ArgPolicy (Intersperse))
import Text.Read qualified as TR

-- | CLI args.
--
-- @since 0.1
data Args = MkArgs
  { pathSizeConfig :: PathSizeConfig,
    path :: FilePath
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''Args

-- | Retrieves CLI args.
--
-- @since 0.1
getArgs :: IO Args
getArgs = OA.execParser parserInfoArgs

parserInfoArgs :: ParserInfo Args
parserInfoArgs =
  ParserInfo
    { infoParser = argsParser,
      infoFullDesc = True,
      infoProgDesc = Chunk desc,
      infoHeader = Chunk headerTxt,
      infoFooter = Chunk footerTxt,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    headerTxt = Just "fs-utils: Provides file-system utils."
    footerTxt = Just $ fromString versNum
    desc =
      Just $
        mconcat
          [ "\nfs-utils allows one to find large files on the file-system. ",
            "In particular, the command will recursively associate a given ",
            "path and all of its subpaths to their respective sizes."
          ]

argsParser :: Parser Args
argsParser =
  MkArgs
    <$> pathSizeConfigParser
    <*> pathParser
    <**> OA.helper
    <**> version

version :: Parser (a -> a)
version = OA.infoOption txt (OA.long "version")
  where
    txt =
      L.intercalate
        "\n"
        [ "SafeRm",
          versNum,
          "Revision: " <> $(GitRev.gitHash),
          "Date: " <> $(GitRev.gitCommitDate)
        ]

versNum :: String
versNum = "Version: " <> $$(PV.packageVersionStringTH "safe-rm.cabal")

pathSizeConfigParser :: Parser PathSizeConfig
pathSizeConfigParser =
  MkPathSizeConfig
    <$> numPathsParser
    <*> strategyParser

numPathsParser :: Parser (Maybe Natural)
numPathsParser =
  OA.optional
    $ OA.option
      readNat
    $ mconcat
      [ OA.long "num-paths",
        OA.short 'n',
        OA.metavar "NAT",
        OA.help helpTxt
      ]
  where
    readNat =
      OA.str >>= \s -> case TR.readMaybe s of
        Just n -> pure n
        Nothing -> fail $ "Could not read natural: " <> s
    helpTxt =
      mconcat
        [ "The number of paths to display. If unspecified, returns all paths."
        ]

strategyParser :: Parser Strategy
strategyParser =
  OA.option
    readStrategy
    $ mconcat
      [ OA.value Sync,
        OA.long "strategy",
        OA.short 's',
        OA.metavar "[sync|async|pool]",
        OA.help helpTxt
      ]
  where
    readStrategy =
      OA.str >>= \case
        "sync" -> pure Sync
        "async" -> pure Async
        "pool" -> pure AsyncPooled
        other ->
          fail $
            mconcat
              [ "Could not read strategy. Wanted one of [sync|async|pool], found: ",
                other
              ]
    helpTxt =
      mconcat
        [ "The search strategy intended to improve performance. The default ",
          "is 'sync', as in, a sequential search. The 'async' option uses ",
          "lightweight threads. Finally, 'pool' uses an explicit thread ",
          "pool for concurrency. This is potentially the fastest, though ",
          "experimentation is recommended."
        ]

pathParser :: Parser FilePath
pathParser = OA.argument OA.str (OA.metavar "PATH")