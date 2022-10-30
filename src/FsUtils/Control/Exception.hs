{-# LANGUAGE ImplicitParams #-}

module FsUtils.Control.Exception
  ( -- * Functions
    withCallStack,
    throwCallStack,

    -- * Exceptions
    ArbitraryE,
  )
where

import Control.Exception
  ( Exception (displayException),
    SomeException,
    catch,
    throwIO,
  )
import GHC.Stack (CallStack, HasCallStack, prettyCallStack)

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
withCallStack action = do
  catch @SomeException action $ \e -> throwIO $ MkArbitraryE e ?callStack

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
