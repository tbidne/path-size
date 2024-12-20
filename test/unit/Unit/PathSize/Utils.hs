{-# LANGUAGE CPP #-}

module Unit.PathSize.Utils (tests) where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import FileSystem.OsPath (OsPath)
import FileSystem.OsPath qualified as FS.OsPath
import Hedgehog (Gen, annotate, forAll, property)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import PathSize.Utils qualified as Utils
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "PathSize.Utils"
    [ testHidden,
      testNotHidden
    ]

testHidden :: TestTree
testHidden = testPropertyNamed desc "testHidden" $ do
  property $ do
    p <- forAll genDotPath
    let p' = FS.OsPath.decodeLenient p

    -- For debugging
    annotate p'

    -- windows is always considered unhidden
    if isWindows
      then H.assert $ not $ Utils.hidden p
      else H.assert $ Utils.hidden p
  where
    desc = "Hidden dir correctly identified"

testNotHidden :: TestTree
testNotHidden = testPropertyNamed desc "testNotHidden" $ do
  property $ do
    p <- forAll genNoDotPath
    let p' = FS.OsPath.decodeLenient p

    -- For debugging
    annotate p'

    H.assert $ not $ Utils.hidden p
  where
    desc = "Visible dir correctly not identified"

-- Generates:
--
-- @
--   <not dot> : ...
-- @
genNoDotPath :: Gen OsPath
genNoDotPath =
  FS.OsPath.unsafeEncode <$> genPath
  where
    genFirst = Gen.filter (/= '.') Gen.unicode
    genPath =
      liftA2
        (:)
        genFirst
        (Gen.string (Range.linear 0 19) Gen.unicode)

-- Generates:
--
-- @
--   . : <not slash> : ...
-- @
genDotPath :: Gen OsPath
genDotPath =
  FS.OsPath.unsafeEncode . ('.' :) <$> genPath
  where
    genFirst = Gen.filter (/= '/') Gen.unicode
    genPath =
      liftA2
        (:)
        genFirst
        (Gen.string (Range.linear 0 18) Gen.unicode)

{- ORMOLU_DISABLE -}

isWindows :: Bool
isWindows =
#if WINDOWS
  True
#else
  False
#endif

{- ORMOLU_ENABLE -}
