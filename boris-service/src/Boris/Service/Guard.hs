{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Service.Guard (
    TerminationAction (..)
  , TerminationHandler (..)
  , guarded
  , repeatedly
  ) where

import           Boris.Service.Snooze
import           Boris.Prelude

import           Control.Monad.Catch (SomeException, catchAll)
import           Control.Monad.Trans.Class

import           System.IO

data TerminationAction =
    Restart
  | Die
  deriving (Eq, Show)

data TerminationHandler e =
  TerminationHandler {
      onExplosion :: SomeException -> IO TerminationAction
    , onError :: e -> IO TerminationAction
    , onGraceful :: IO TerminationAction
    }

-- | Run an action forever, using termination handler to notify of
--   failure events.
--
--   This is generally most useful to guard a thread against
--   un-noticed termination.
--
--   A reasonable usage would be to add monitoring / notifications
--   in termination handler and just let this loop keep your code
--   alive. It is recommened that you use the termination handler
--   to control number of retried.
--
--   An equally reasonable alternative is to call exitImmediately
--   (or some equivalent - perhaps you have an MVar controlling
--   program termination?), and use this to ensure that the entire
--   process dies if any of the supervised threads die.
--
--   The action passed to 'guard' should run forever, if you
--   want to run a short-lived action repeatedly in a supervised
--   fashion see 'repeatedly'. This function will still work, but
--   the 'onGraceful' handler will be called (a lot).
--
--   Common usage (where expectation is the do block never return):
--   @
--     void . forkIO . guard (TerminationHandler ...) . forever $ do
--       doThis
--       andThis
--   @
--
--
guarded :: TerminationHandler e -> EitherT e IO () -> IO ()
guarded handler action =
  let run = runEitherT action >>= either (onError handler) (const . onGraceful $ handler)
      safe = run `catchAll` onExplosion handler `catchAll` (const . pure) Restart
   in safe >>= \a -> when (a == Restart) $ guarded handler action

-- | Run an action repeatedly with a fixed delay between runs, using
--   termination handler to notify of failure events.
--
--   See 'guard' for more description. This offers the same
--   behaviour for actions that don't run forever.
--
--   Common usage (where expectation is the do block does returns):
--   @
--     void . forkIO . repeatedly (seconds 5) (TerminationHandler ...) $ do
--       doThis
--       andThis
--   @
--
repeatedly :: Duration -> TerminationHandler e -> EitherT e IO () -> IO ()
repeatedly d handler action =
  let ever = action >> lift (snooze d) >> ever
   in guarded handler ever
