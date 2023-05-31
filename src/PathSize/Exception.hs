-- | Provides exception types/functions for usage with path-size.
--
-- @since 0.1
module PathSize.Exception
  ( PathE (..),
  )
where

import Control.DeepSeq (NFData)
import Control.Exception (Exception (displayException))
import Effects.FileSystem.Path (Path)
import GHC.Generics (Generic)

-- | Exception for a path. The second param is the reason i.e. the exceptions'
-- displayException. The reason we convert to a string rather than leave it
-- as an exception is so we can have an NFData instance for benchmarking.
--
-- @since 0.1
data PathE = MkPathE !Path !String
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance Exception PathE where
  displayException (MkPathE p e) =
    mconcat
      [ "Path: ",
        show p,
        ". Error: ",
        e
      ]
