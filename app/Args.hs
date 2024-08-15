-- | Provides CLI args functionality.
--
-- @since 0.1
module Args
  ( Args (..),
    getArgs,
    argsToConfig,
  )
where

import Control.Monad ((>=>))
import Data.HashSet (HashSet)
import Data.HashSet qualified as HSet
import Data.List qualified as L
import Data.String (IsString (fromString))
import Data.Version (Version (versionBranch))
import Data.Word (Word16)
import Effects.FileSystem.Utils (OsPath)
import Effects.Optparse (osPath)
import Numeric.Data.Positive (Positive, mkPositive)
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
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse))
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
  )
import PathSize.Data.Config (defaultNumPaths)
import Paths_path_size qualified as Paths
import Text.Read qualified as TR

-- | CLI args.
--
-- @since 0.1
data Args = MkArgs
  { searchAll :: !Bool,
    maxDepth :: !(Maybe Word16),
    exclude :: !(HashSet OsPath),
    filesOnly :: !Bool,
    ignoreDirIntrinsicSize :: !Bool,
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
    footerTxt = Just $ fromString versNum
    desc =
      Chunk.paragraph $
        mconcat
          [ "path-size allows one to find large paths on the file-system. ",
            "In particular, the command will recursively associate a given ",
            "path and all of its subpaths to their respective sizes."
          ]

argsParser :: Parser Args
argsParser =
  MkArgs
    <$> allParser
    <*> depthParser
    <*> excludeParser
    <*> filesOnlyParser
    <*> ignoreDirIntrinsicSizeParser
    <*> numPathsParser
    <*> reverseSortParser
    <*> stableSortParser
    <*> strategyParser
      <**> OA.helper
      <**> version
    <*> pathParser

version :: Parser (a -> a)
version = OA.infoOption versNum (OA.long "version" <> OA.short 'v')

versNum :: String
versNum = "Version: " <> L.intercalate "." (show <$> versionBranch Paths.version)

numPathsParser :: Parser (Maybe (Positive Int))
numPathsParser =
  OA.option
    readNat
    $ mconcat
      [ OA.value (Just defaultNumPaths),
        OA.long "num-paths",
        OA.short 'n',
        OA.metavar "(NAT | all)",
        mkHelp helpTxt
      ]
  where
    readNat =
      OA.str >>= \case
        "all" -> pure Nothing
        s -> case (TR.readMaybe >=> mkPositive) s of
          Just n -> pure $ Just n
          Nothing -> fail $ "Expected 'all' or a positive, received: " <> s
    helpTxt =
      mconcat
        [ "The number of paths to display. If unspecified, defaults to 10. ",
          "The option 'all' returns everything."
        ]

excludeParser :: Parser (HashSet OsPath)
excludeParser =
  HSet.fromList
    <$> OA.many
      ( OA.option
          osPath
          $ mconcat
            [ OA.long "exclude",
              OA.short 'e',
              OA.metavar "PATHS...",
              mkHelp helpTxt
            ]
      )
  where
    helpTxt =
      mconcat
        [ "Paths to skip. These must match the desired directory/file name ",
          "e.g. to skip /path/to/dir you would pass '-e dir'. Note that this ",
          "will exclude _all_ subpaths that match 'dir'."
        ]

allParser :: Parser Bool
allParser =
  OA.switch $
    mconcat
      [ OA.long "all",
        OA.short 'a',
        mkHelp helpTxt
      ]
  where
    helpTxt = "If enabled, searches hidden files/directories."

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
        [ "If enabled, only sizes for files are calculated. All directories ",
          "are given size 0. Note this effectively implies --ignore-dir-size."
        ]

ignoreDirIntrinsicSizeParser :: Parser Bool
ignoreDirIntrinsicSizeParser =
  OA.switch $
    mconcat
      [ OA.long "ignore-dir-size",
        mkHelp helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "If enabled, ignores the size of the directories themselves i.e. ",
          "a directory's size is determined by the sum of all of its subfiles, ",
          "only. The size of the directory itself (e.g. 4096 bytes on a ",
          " typical ext4 filesystem) is ignored."
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
          "implicitly included in parent directories, but not directly."
        ]

reverseSortParser :: Parser Bool
reverseSortParser =
  OA.switch $
    mconcat
      [ OA.long "reverse",
        OA.short 'r',
        mkHelp helpTxt
      ]
  where
    helpTxt = "If enabled, paths are sorted in reverse (ascending) order."

stableSortParser :: Parser Bool
stableSortParser =
  OA.switch $
    mconcat
      [ OA.long "stable",
        mkHelp helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "If enabled, an additional sorting filter is applied to sort by path ",
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
        OA.metavar "(async|sync|pool)",
        mkHelp helpTxt
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
              [ "Could not read strategy. Wanted one of [sync|async|pool], found: ",
                other
              ]
    helpTxt =
      mconcat
        [ "The search strategy is intended to improve performance. The ",
          "default is 'async', which uses lightweight threads. The 'sync' ",
          "option is a sequential search and likely the slowest. Finally, ",
          "'pool' uses an explicit thread pool for concurrency. This is ",
          "potentially the fastest, though experimentation is recommended."
        ]

pathParser :: Parser OsPath
pathParser = OA.argument osPath (OA.metavar "PATH")

mkHelp :: String -> OA.Mod f a
mkHelp =
  OA.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph
