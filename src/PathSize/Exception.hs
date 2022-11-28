{-# LANGUAGE ImplicitParams #-}

-- | Provides exception types/functions for usage with path-size.
--
-- @since 0.1
module PathSize.Exception
  ( -- * Functions
    withCallStack,
    throwCallStack,

    -- * Exceptions
    PathE (..),
    ArbitraryE,
  )
where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import GHC.Stack (CallStack, HasCallStack, prettyCallStack)
import UnliftIO.Exception
  ( Exception (displayException),
    SomeException,
    catchAny,
    throwIO,
  )

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

-- | Wraps a 'SomeException' with a 'CallStack.
--
-- @since 0.1
data ArbitraryE = MkArbitraryE !SomeException !CallStack
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception ArbitraryE where
  displayException (MkArbitraryE e cs) =
    mconcat
      [ "Exception:\n",
        displayException e,
        appendPrettyCs cs
      ]

-- | Runs the action. If an exception is thrown, wraps the exception in
-- an 'ArbitraryE' and rethrows.
--
-- @since 0.1
withCallStack :: HasCallStack => IO a -> IO a
withCallStack action =
  catchAny action $ \e -> throwIO $ MkArbitraryE e ?callStack

-- | Throws with 'CallStack'.
--
-- @since 0.1
throwCallStack :: (Exception e, HasCallStack) => (CallStack -> e) -> IO a
throwCallStack withCS = throwIO $ withCS ?callStack

appendPrettyCs :: CallStack -> String
appendPrettyCs cs =
  mconcat
    [ "\n\n",
      prettyCallStack cs
    ]
