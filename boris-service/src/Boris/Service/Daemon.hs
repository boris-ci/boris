{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Daemon (
   run
  ) where

import           Boris.Core.Data.Workspace
import           Boris.Service.Boot
import           Boris.Service.Listener
import           Boris.Prelude

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.IO (IO)

import           Boris.Git.Pin (Pin, checkPin)
--import           Twine.Guard (TerminationAction (..), TerminationHandler (..), repeatedly)
--import           Twine.Snooze (seconds)


run :: LogService -> BuildService -> DiscoverService -> WorkspacePath -> Pin -> IO ()
run logs builds discovers work pin = do
  error "todo"
  {--
  repeatedly (seconds 1) (handler "listener" renderListenerError) $
    let go = listen logs builds discovers env q work in liftIO (checkPin pin) >>= flip unless go
--}

{--
handler :: Text -> (e -> Text) -> TerminationHandler e
handler label render =
  TerminationHandler {
      onExplosion = \e -> do
        Restart <$ (T.putStrLn . mconcat)
          ["The supervised thread ", label, " unexpectedly exploded: ", T.pack . show $ e]
    , onError = \e ->
       Restart <$ (T.putStrLn . mconcat)
          ["The supervised thread ", label, " unexpectedly errored: ", render e]
    , onGraceful =
       Die <$ (T.putStrLn . mconcat)
          ["The supervised thread ", label, " has been stopped gracefully."]
    }
--}
