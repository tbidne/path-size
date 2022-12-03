-- | Provides exception types/functions for usage with path-size.
--
-- @since 0.1
module PathSize.Exception
  ( PathE (..),
  )
where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import UnliftIO.Exception (Exception (displayException))

-- | Exception for a path. The second param is the reason i.e. the exceptions'
-- displayException. The reason we convert to a string rather than leave it
-- as an exception is so we can have an NFData instance for benchmarking.
--
-- @since 0.1
data PathE = MkPathE !FilePath !String
  deriving stock
    ( -- | @since 0.1
      Generic,
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
      [ "Exception for path '",
        p,
        "':\n",
        e,
        "\n"
      ]
