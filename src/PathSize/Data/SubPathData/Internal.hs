{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal module for 'SubPathData'. Provides the invariant-breaking
-- data constructor for 'SubPathData'.
--
-- @since 0.1
module PathSize.Data.SubPathData.Internal
  ( SubPathData (.., MkSubPathData),
    unSubPathData,
    mkSubPathData,
    takeLargestN,

    -- * Display
    DisplayConfig (..),
    defaultDisplayConfig,
    DisplayFormat (..),
    display,

    -- ** Optics
    _DisplayFormatSingle,
    _DisplayFormatTabular,
  )
where

import Control.DeepSeq (NFData)
import Data.Bytes
  ( Bytes (MkBytes),
    FloatingFormatter (MkFloatingFormatter),
    Normalize (normalize),
    Size (B),
  )
import Data.Bytes qualified as Bytes
#if !MIN_VERSION_base(4, 20, 0)
import Data.Foldable (Foldable (foldl'))
#endif
import Data.List qualified as L
import Data.Ord (Down (Down))
import Data.Sequence (Seq ((:<|)), (<|))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Builder.Linear (Builder)
import Data.Text.Builder.Linear qualified as TBLinear
import FileSystem.OsPath (OsPath)
import FileSystem.OsPath qualified as OsP
import FileSystem.UTF8 qualified as UTF8
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import GHC.Stack (HasCallStack)
import Numeric.Data.Positive (Positive (MkPositive))
import Optics.Core
  ( A_Getter,
    A_Lens,
    LabelOptic (labelOptic),
    Prism',
    lensVL,
    prism,
    to,
  )
import PathSize.Data.PathData
  ( PathData
      ( MkPathData,
        numDirectories,
        numFiles,
        path,
        size
      ),
  )
import PathSize.Data.PathTree (PathTree, pathTreeToSeq)
#if POSIX
import Data.ByteString.Short qualified as BS.Short
import System.OsString.Internal.Types
  ( OsString (getOsString),
    PosixString(getPosixString),
  )
import FileSystem.UTF8 qualified as FS.UTF8
#else
import FileSystem.OsPath qualified as FS.OsPath
#endif

-- | A flattened and sorted representation of 'PathTree'. Contains at least
-- one element.
--
-- @since 0.1
newtype SubPathData = UnsafeSubPathData (NESeq PathData)
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
instance HasField "unSubPathData" SubPathData (NESeq PathData) where
  getField (UnsafeSubPathData xs) = xs

-- NOTE: This is hand-written because we want to use labeled optics and the
-- obvious strategy:
--
-- 1. Record selector unSubPathData
-- 2. TH generation
--
-- will require us to make the ordinary unSubPathData eliminator into an
-- unsafe record updater, which we do not want.

-- | @since 0.1
instance
  LabelOptic
    "unSubPathData"
    A_Getter
    SubPathData
    SubPathData
    (NESeq PathData)
    (NESeq PathData)
  where
  labelOptic = to (\(UnsafeSubPathData sbd) -> sbd)

-- | Pattern synonym for 'SubPathData'. Note that construction sorts the
-- underlying 'NESeq', so it is not constant.
--
-- @since 0.1
pattern MkSubPathData :: NESeq PathData -> SubPathData
pattern MkSubPathData sbd <- UnsafeSubPathData sbd
  where
    MkSubPathData sbd = UnsafeSubPathData (sortNESeq False sbd)

{-# COMPLETE MkSubPathData #-}

-- | @since 0.1
unSubPathData :: SubPathData -> NESeq PathData
unSubPathData (UnsafeSubPathData sbd) = sbd

-- | Creates a 'SubPathData' from a 'PathTree'.
--
-- @since 0.1
mkSubPathData :: Bool -> PathTree -> SubPathData
mkSubPathData stableSort tree = UnsafeSubPathData (first :<|| rest)
  where
    first :<|| rest = sortNESeq stableSort (pathTreeToSeq tree)

-- | Returns a 'Seq' representation of 'SubPathData'.
--
-- @since 0.1
subPathDataToSeq :: SubPathData -> Seq PathData
subPathDataToSeq (UnsafeSubPathData (pd :<|| xs)) = pd <| xs

-- NOTE: Annoyingly, this sort seems to cost quite a bit of performance over
-- the previous (Down . view #size). It is now applying an additional sort
-- to the path. This was done initially to make testing easier (stable order
-- for paths w/ identical sizes), but the added determinism + usability
-- seems worth it.

-- | Sorts the path size.
--
-- @since 0.1
sortNESeq :: Bool -> NESeq PathData -> NESeq PathData
sortNESeq False = NESeq.sortOn pathDataSizeOrd
sortNESeq True = NESeq.sortOn pathDataSizePathOrd

pathDataSizeOrd :: PathData -> Down Integer
pathDataSizeOrd = Down . (.size)

pathDataSizePathOrd :: PathData -> Down (Integer, OsPath)
pathDataSizePathOrd = Down . \(MkPathData p s _ _) -> (s, p)

{- HLINT ignore takeLargestN "Redundant bracket" -}

-- | Retrieves the largest N paths.
--
-- @since 0.1
takeLargestN :: (HasCallStack) => Bool -> Positive Int -> PathTree -> SubPathData
takeLargestN stableSort (MkPositive n) tree = case NESeq.take n sorted of
  (first :<| rest) -> UnsafeSubPathData (first :<|| rest)
  -- NOTE: Should only happen if n == 0
  _ ->
    error $
      mconcat
        [ "[PathSize.Data.SubPathData.Internal.takeLargestN]: ",
          "impossible, returned empty Seq for i = ",
          show n,
          ", tree = ",
          show tree
        ]
  where
    sorted = sortNESeq stableSort (pathTreeToSeq tree)

-- | @since 0.1
data DisplayFormat
  = DisplayFormatSingle
  | DisplayFormatTabular
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
_DisplayFormatSingle :: Prism' DisplayFormat ()
_DisplayFormatSingle =
  prism
    (const DisplayFormatSingle)
    ( \case
        DisplayFormatSingle -> Right ()
        x -> Left x
    )
{-# INLINE _DisplayFormatSingle #-}

-- | @since 0.1
_DisplayFormatTabular :: Prism' DisplayFormat ()
_DisplayFormatTabular =
  prism
    (const DisplayFormatTabular)
    ( \case
        DisplayFormatTabular -> Right ()
        x -> Left x
    )
{-# INLINE _DisplayFormatTabular #-}

-- | @since 0.1
data DisplayConfig = MkDisplayConfig
  { -- | @since 0.1
    color :: Bool,
    -- | @since 0.1
    format :: DisplayFormat,
    -- | @since 0.1
    reverseSort :: Bool
  }
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

defaultDisplayConfig :: DisplayConfig
defaultDisplayConfig =
  MkDisplayConfig
    { color = True,
      format = DisplayFormatTabular,
      reverseSort = False
    }

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "color" k DisplayConfig DisplayConfig a b
  where
  labelOptic =
    lensVL $
      \f (MkDisplayConfig x1 x2 x3) ->
        fmap (\b -> MkDisplayConfig b x2 x3) (f x1)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ DisplayFormat, b ~ DisplayFormat) =>
  LabelOptic "format" k DisplayConfig DisplayConfig a b
  where
  labelOptic =
    lensVL $
      \f (MkDisplayConfig x1 x2 x3) ->
        fmap (\b -> MkDisplayConfig x1 b x3) (f x2)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "reverseSort" k DisplayConfig DisplayConfig a b
  where
  labelOptic =
    lensVL $
      \f (MkDisplayConfig x1 x2 x3) ->
        fmap (\b -> MkDisplayConfig x1 x2 b) (f x3)
  {-# INLINE labelOptic #-}

-- | Displays the data.
--
-- @since 0.1
display :: DisplayConfig -> SubPathData -> Text
display (MkDisplayConfig {color, format, reverseSort}) spd =
  TBLinear.runBuilder $ case format of
    DisplayFormatSingle ->
      if color
        then
          foldSeq goColor ""
            . Seq.zip colorSeq
            $ xs
        else foldSeq go "" $ xs
    DisplayFormatTabular ->
      if color
        then
          (colorTxt green tableHeader <>)
            . foldSeq goTableColor ""
            . Seq.zip colorSeq
            $ xs
        else (tableHeader <>) . foldSeq goTable "" $ xs
  where
    xs = subPathDataToSeq $ spd

    go :: PathData -> Builder -> Builder
    go (MkPathData {path, size, numFiles, numDirectories}) acc =
      mconcat
        [ TBLinear.fromText $ pathToText path,
          ": ",
          TBLinear.fromText $ formatSize size,
          ", Directories: ",
          TBLinear.fromText $ formatInt numDirectories,
          ", Files: ",
          TBLinear.fromText $ formatInt numFiles,
          "\n",
          acc
        ]

    goColor :: (Builder, PathData) -> Builder -> Builder
    goColor (lineColor, MkPathData {path, size, numFiles, numDirectories}) acc =
      mconcat
        [ lineColor,
          TBLinear.fromText $ pathToText path,
          ": ",
          TBLinear.fromText $ formatSize size,
          ", Directories: ",
          TBLinear.fromText $ formatInt numDirectories,
          ", Files: ",
          TBLinear.fromText $ formatInt numFiles,
          endColor,
          "\n",
          acc
        ]

    tableHeader =
      mconcat
        [ TBLinear.fromText $ lpadN maxPathLen "Path",
          sep,
          TBLinear.fromText $ rpadN maxSizeLen "Size",
          sep,
          TBLinear.fromText $ rpadN maxDirLen "Dirs",
          sep,
          TBLinear.fromText $ rpadN maxFileLen "Files",
          "\n",
          TBLinear.fromText $ hyphens,
          "\n"
        ]

    -- Sep x 3 == 3 x 3 == 9
    hyphens = T.replicate (9 + padSize + padDir + padFile + padPath) "-"

    goTable :: PathData -> Builder -> Builder
    goTable (MkPathData {path, size, numFiles, numDirectories}) acc =
      mconcat
        [ TBLinear.fromText $ lpadN padPath $ pathToText path,
          sep,
          TBLinear.fromText $ rpadN padSize $ formatSize size,
          sep,
          TBLinear.fromText $ rpadN padDir $ formatInt numDirectories,
          sep,
          TBLinear.fromText $ rpadN padFile $ formatInt numFiles,
          "\n",
          acc
        ]

    goTableColor :: (Builder, PathData) -> Builder -> Builder
    goTableColor (lineColor, MkPathData {path, size, numFiles, numDirectories}) acc =
      mconcat
        [ lineColor,
          TBLinear.fromText $ lpadN padPath $ pathToText path,
          sep,
          TBLinear.fromText $ rpadN padSize $ formatSize size,
          sep,
          TBLinear.fromText $ rpadN padDir $ formatInt numDirectories,
          sep,
          TBLinear.fromText $ rpadN padFile $ formatInt numFiles,
          endColor,
          "\n",
          acc
        ]

    padSize = max maxSizeLen 4
    padDir = max maxDirLen 4
    padFile = max maxFileLen 5
    padPath = max 4 maxPathLen

    -- We want the max size of the _displayed text_, not necessarily the
    -- number itself. For dir and file len, the maxium number will also have
    -- the maximum text length, so we can just take the largest number and
    -- convert after, which is more efficient. With size and path length,
    -- however, we cannot do this:
    --
    --   - For size, we may have e.g. 36.28G > 994.72M, but the latter is
    --     syntactically longer. Hence we must convert first.
    --
    --   - For paths, we have some complexity regarding unicode, hence it
    --     is simplest to just compare the final conversions.
    (maxSizeLen, maxDirLen, maxFileLen, maxPathLen) =
      let k :: (Int, Integer, Integer, Int) -> PathData -> (Int, Integer, Integer, Int)
          k (!s, !d, !f, !p) (MkPathData {size, path, numFiles, numDirectories}) =
            ( max s (T.length $ formatSize size),
              max d numDirectories,
              max f numFiles,
              max p (pathLength path)
            )
          (maxSz, maxDs, maxFs, maxPs) = foldl' k (0, 0, 0, 0) xs
       in ( maxSz,
            T.length $ formatInt maxDs,
            T.length $ formatInt maxFs,
            maxPs
          )

    sep = " | "

    lpadN = padN (flip (<>))

    -- Right pad. Left pad would be 'flip (<>)'.
    rpadN :: Int -> Text -> Text
    rpadN = padN (<>)

    padN :: (Text -> Text -> Text) -> Int -> Text -> Text
    padN padFn n s
      | d < 0 = s
      | otherwise = ws `padFn` s
      where
        d = n - T.length s
        ws = T.replicate d " "

    formatSize :: Integer -> Text
    formatSize =
      Bytes.formatSized
        (MkFloatingFormatter (Just 2))
        Bytes.sizedFormatterUnix
        . normalize
        . MkBytes @B
        . fromIntegral @_ @Double

    foldSeq :: forall a. (a -> Builder -> Builder) -> Builder -> Seq a -> Builder
    foldSeq
      | reverseSort = foldl' . flip
      | otherwise = foldr

    blue = "\ESC[34m"

    green = "\ESC[32m"

    magenta = "\ESC[35m"

    endColor = "\ESC[0m"

    colorTxt c m = c <> m <> endColor

    colorSeq = Seq.fromList (L.take (length xs) colorStream)
    colorStream = blue : magenta : colorStream

-- We use the length for visual alignment, hence want displayed characters.
-- Thus we normalize it first to account for grapheme clusters.
--
-- E.g. we want the string ['\x4F', '\x308'] ("Ö") to have length 1.
--
-- Note that there is a separate issue where Windows + GHC 9.10
-- (os-string < 2.0.3) counts number of bytes, rather than number of word16
-- byte boundaries. Therefore a simpler solution (that ignores grapheme
-- clusters) could use os-string length but would need to guard the GHC and
-- os-string version. Simpler still (and probably decent) would be to just
-- count text length.
pathLength :: OsPath -> Int
pathLength = OsP.glyphLength

formatInt :: Integer -> Text
formatInt =
  T.reverse
    . T.intercalate "_"
    . T.chunksOf 3
    . T.reverse
    . T.pack
    . show

pathToText :: OsPath -> Text

#if POSIX
pathToText =
  UTF8.normalizeC
    . FS.UTF8.decodeUtf8Lenient
    . BS.Short.fromShort
    . (\p -> p.getOsString.getPosixString)
#else
pathToText = UTF8.normalizeC . T.pack . FS.OsPath.decodeLenient
#endif
