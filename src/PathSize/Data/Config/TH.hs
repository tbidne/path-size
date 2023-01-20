{-# LANGUAGE TemplateHaskell #-}

-- | Provides TH for Config.
--
-- @since 0.1
module PathSize.Data.Config.TH
  ( defaultNumPaths,
    defaultNumPathsSize,
  )
where

import Numeric.Data.Positive (Positive, mkPositiveTH)

-- | Default num paths for normal, full search.
--
-- @since 0.1
defaultNumPaths :: Positive Int
defaultNumPaths = $$(mkPositiveTH 10)

-- | Default num paths for size search.
--
-- @since 0.1
defaultNumPathsSize :: Positive Int
defaultNumPathsSize = $$(mkPositiveTH 1)
