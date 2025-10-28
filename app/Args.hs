{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Provides CLI args functionality.
--
-- @since 0.1
module Args
  ( Args (..),
    getArgs,
    argsToConfig,
    argsToDisplayConfig,
  )
where

import Args.TH qualified as TH
import Control.Monad ((>=>))
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String (IsString (fromString))
import Data.Version (showVersion)
import Data.Word (Word16)
import Effects.Optparse (osPath)
import Effects.Optparse.Completer qualified as EOC
import FileSystem.OsPath (OsPath)
import FileSystem.OsString (OsString)
import FileSystem.OsString qualified as OsString
import Numeric.Data.Positive (Positive, mkPositive)
import Options.Applicative
  ( Mod,
    OptionFields,
    Parser,
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
import Options.Applicative.Help (Doc)
import Options.Applicative.Help.Chunk (Chunk (Chunk))
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse))
import PathSize
  ( DisplayConfig (MkDisplayConfig, color, format, reverseSort),
    DisplayFormat (DisplayFormatSingle, DisplayFormatTabular),
  )
import PathSize.Data.Config
  ( Config
      ( MkConfig,
        exclude,
        filesOnly,
        ignoreDirIntrinsicSize,
        maxDepth,
        numPaths,
        searchAll,
        stableSort,
        strategy
      ),
    Strategy (Async, AsyncPool, Sync),
    defaultNumPaths,
  )
import Paths_path_size qualified as Paths
import System.FilePath.Glob (Pattern)
import System.FilePath.Glob qualified as Glob
import System.Info qualified as Info
import Text.Read qualified as TR

-- | CLI args.
--
-- @since 0.1
data Args = MkArgs
  { searchAll :: !Bool,
    maxDepth :: !(Maybe Word16),
    exclude :: ![Pattern],
    filesOnly :: !Bool,
    format :: !DisplayFormat,
    ignoreDirIntrinsicSize :: !Bool,
    noColor :: !Bool,
    numPaths :: !(Maybe (Positive Int)),
    reverseSort :: !Bool,
    stableSort :: !Bool,
    strategy :: !Strategy,
    path :: !OsPath
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

argsToConfig :: Args -> Config
argsToConfig args =
  MkConfig
    { searchAll = args.searchAll,
      maxDepth = args.maxDepth,
      exclude = args.exclude,
      filesOnly = args.filesOnly,
      ignoreDirIntrinsicSize = args.ignoreDirIntrinsicSize,
      numPaths = args.numPaths,
      stableSort = args.stableSort,
      strategy = args.strategy
    }

argsToDisplayConfig :: Args -> DisplayConfig
argsToDisplayConfig args =
  MkDisplayConfig
    { color = not args.noColor,
      format = args.format,
      reverseSort = args.reverseSort
    }

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
      infoProgDesc = desc,
      infoHeader = Chunk headerTxt,
      infoFooter = Chunk footerTxt,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    headerTxt =
      Just
        "path-size: A utility for reporting the recursive size of a directory."
    footerTxt = Just $ fromString versShort
    desc =
      Chunk.paragraph $
        mconcat
          [ "path-size allows one to find large paths on the file-system. ",
            "In particular, the command will recursively associate a given ",
            "path and all of its subpaths to their respective sizes."
          ]

argsParser :: Parser Args
argsParser = p <**> version <**> OA.helper
  where
    p = do
      ~(filesOnly, ignoreDirIntrinsicSize) <- parseDirGroup
      ~(format, noColor, numPaths, reverseSort, stableSort) <- parseFormattingGroup
      strategy <- parseMiscGroup
      ~(searchAll, maxDepth, exclude) <- parseSearchGroup
      path <- pathParser
      pure $
        MkArgs
          { searchAll,
            maxDepth,
            exclude,
            filesOnly,
            format,
            ignoreDirIntrinsicSize,
            noColor,
            numPaths,
            reverseSort,
            stableSort,
            strategy,
            path
          }

    parseDirGroup =
      OA.parserOptionGroup "Directory options:" $
        (,) <$> filesOnlyParser <*> ignoreDirIntrinsicSizeParser

    parseFormattingGroup =
      OA.parserOptionGroup "Formatting options:" $
        (,,,,)
          <$> formatParser
          <*> noColorParser
          <*> numPathsParser
          <*> reverseSortParser
          <*> stableSortParser

    parseMiscGroup =
      OA.parserOptionGroup "Miscellaneous options:" strategyParser

    parseSearchGroup =
      OA.parserOptionGroup "Search options:" $
        (,,) <$> allParser <*> depthParser <*> excludeParser

version :: Parser (a -> a)
version = OA.infoOption versLong (OA.long "version" <> OA.short 'v' <> OA.hidden)

versShort :: String
versShort =
  mconcat
    [ "Version: ",
      showVersion Paths.version,
      " (",
      OsString.decodeLenient versionInfo.gitShortHash,
      ")"
    ]

versLong :: String
versLong =
  L.intercalate
    "\n"
    [ "Path-size: " <> showVersion Paths.version,
      " - Git revision: " <> OsString.decodeLenient versionInfo.gitHash,
      " - Commit date:  " <> OsString.decodeLenient versionInfo.gitCommitDate,
      " - GHC version:  " <> versionInfo.ghc
    ]

data VersionInfo = MkVersionInfo
  { gitCommitDate :: OsString,
    ghc :: String,
    gitHash :: OsString,
    gitShortHash :: OsString
  }

versionInfo :: VersionInfo
versionInfo =
  MkVersionInfo
    { gitCommitDate = d,
      ghc = showVersion Info.compilerVersion,
      gitHash = h,
      gitShortHash = sh
    }
  where
    (d, h, sh) = $$TH.gitData

numPathsParser :: Parser (Maybe (Positive Int))
numPathsParser =
  OA.option
    readNat
    $ mconcat
      [ OA.value (Just defaultNumPaths),
        OA.long "num-paths",
        OA.short 'n',
        OA.completeWith ["all"],
        OA.metavar "(NAT | all)",
        mkHelp helpTxt
      ]
  where
    readNat =
      OA.str >>= \case
        "all" -> pure Nothing
        s -> case (TR.readEither >=> mkPositive) s of
          Right n -> pure $ Just n
          Left err -> fail $ "Expected 'all' or a positive, error: " <> err
    helpTxt =
      mconcat
        [ "The number of paths to display. If unspecified, defaults to 10. ",
          "The option 'all' returns everything."
        ]

excludeParser :: Parser [Pattern]
excludeParser =
  fmap Glob.compile
    <$> OA.many
      ( OA.option
          OA.str
          $ mconcat
            [ OA.long "exclude",
              OA.short 'e',
              OA.metavar "Patterns...",
              mkHelpNoLine helpTxt
            ]
      )
  where
    helpTxt = "Glob patterns to skip."

allParser :: Parser Bool
allParser =
  OA.switch $
    mconcat
      [ OA.long "all",
        OA.short 'a',
        mkHelp helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "Searches hidden files/directories. We only consider ",
          "hidden files per the unix dot convention (e.g. .hidden_path). ",
          "All files are considered unhidden on windows."
        ]

filesOnlyParser :: Parser Bool
filesOnlyParser =
  OA.switch $
    mconcat
      [ OA.long "files-only",
        OA.short 'f',
        mkHelp helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "Only sizes for files are calculated. All directories ",
          "are given size 0. Note this effectively implies --ignore-dir-size."
        ]

ignoreDirIntrinsicSizeParser :: Parser Bool
ignoreDirIntrinsicSizeParser =
  OA.switch $
    mconcat
      [ OA.long "ignore-dir-size",
        mkHelpNoLine helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "Ignores the size of the directories themselves i.e. ",
          "a directory's size is determined by the sum of all of its subfiles, ",
          "only. The size of the directory itself (e.g. 4096 bytes on a ",
          "typical ext4 filesystem) is ignored."
        ]

depthParser :: Parser (Maybe Word16)
depthParser =
  OA.optional
    $ OA.option
      readNat
    $ mconcat
      [ OA.long "depth",
        OA.short 'd',
        OA.metavar "NAT",
        mkHelp helpTxt
      ]
  where
    readNat =
      OA.str >>= \s -> case TR.readMaybe s of
        Just n -> pure n
        Nothing -> fail $ "Could not read natural: " <> s
    helpTxt =
      mconcat
        [ "The depth limit of our search. Note that we still need to fully ",
          "traverse the file system to get accurate data; this argument ",
          "merely affects what is reported i.e. any depths > d are ",
          "implicitly included in parent directories via their size, but are ",
          "not directly reported themselves."
        ]

noColorParser :: Parser Bool
noColorParser =
  OA.switch $
    mconcat
      [ OA.long "no-color",
        mkHelp "Disables output colors."
      ]

formatParser :: Parser DisplayFormat
formatParser =
  OA.option
    readFormat
    $ mconcat
      [ OA.value DisplayFormatTabular,
        OA.long "format",
        OA.completeWith ["single", "tabular"],
        OA.metavar "FMT",
        helpTxt
      ]
  where
    readFormat =
      OA.str >>= \case
        "single" -> pure DisplayFormatSingle
        "tabular" -> pure DisplayFormatTabular
        other -> fail $ "Unrecognized format: " ++ other

    helpTxt =
      itemize
        [ intro,
          single,
          tabular
        ]

    intro = "Formatting options."
    single = "single: Simple, single-line format."
    tabular = "tabular: The default. Prints a table."

reverseSortParser :: Parser Bool
reverseSortParser =
  OA.switch $
    mconcat
      [ OA.long "reverse",
        OA.short 'r',
        mkHelp helpTxt
      ]
  where
    helpTxt = "Paths are sorted in reverse (ascending) order."

stableSortParser :: Parser Bool
stableSortParser =
  OA.switch $
    mconcat
      [ OA.long "stable",
        mkHelpNoLine helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "An additional sorting filter is applied to sort by path ",
          "name. This allows the sorted order to be deterministic (as paths are ",
          "unique), at the cost of performance."
        ]

strategyParser :: Parser Strategy
strategyParser =
  OA.option
    readStrategy
    $ mconcat
      [ OA.value Async,
        OA.long "strategy",
        OA.short 's',
        OA.completeWith ["async", "sync", "pool"],
        OA.metavar "(async | sync | pool)",
        helpTxt
      ]
  where
    readStrategy =
      OA.str >>= \case
        "sync" -> pure Sync
        "async" -> pure Async
        "pool" -> pure AsyncPool
        other ->
          fail $
            mconcat
              [ "Could not read strategy. Wanted one of (async | pool | sync), found: ",
                other
              ]

    helpTxt =
      itemizeNoLine
        [ intro,
          async,
          pool,
          sync
        ]

    intro = "Search strategy options, for improved performance."
    async = "async: The default, uses lightweight threads."
    pool = "pool: Uses an explicit thread pool. Potentially the fastest."
    sync = "sync: Sequential search, likely the slowest."

pathParser :: Parser OsPath
pathParser =
  OA.argument osPath $
    OA.metavar "PATH"
      <> OA.completer EOC.compgenCwdPathsCompleter

mkHelp :: String -> Mod f a
mkHelp =
  OA.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph

mkHelpNoLine :: String -> OA.Mod f a
mkHelpNoLine =
  OA.helpDoc
    . Chunk.unChunk
    . Chunk.paragraph

toMDoc :: String -> Maybe Doc
toMDoc = Chunk.unChunk . Chunk.paragraph

itemize :: NonEmpty String -> Mod OptionFields a
itemize =
  OA.helpDoc
    . Chunk.unChunk
    . fmap (<> Pretty.line)
    . itemizeHelper

-- | 'itemize' that does not append a trailing newline. Useful for the last
-- option in a group, as groups already start a newline.
itemizeNoLine :: NonEmpty String -> Mod OptionFields a
itemizeNoLine =
  OA.helpDoc
    . Chunk.unChunk
    . itemizeHelper

itemizeHelper :: NonEmpty String -> Chunk Doc
itemizeHelper (intro :| ds) =
  Chunk.vcatChunks
    ( Chunk.paragraph intro
        : toChunk Pretty.softline
        : (toItem <$> ds)
    )
  where
    toItem d =
      fmap (Pretty.nest 2)
        . Chunk.paragraph
        $ ("- " <> d)

toChunk :: a -> Chunk a
toChunk = Chunk . Just
