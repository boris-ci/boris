{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Daemon (
   run
  ) where

import           Boris.Core.Data.Workspace
import           Boris.Client.Config (Boris)
import           Boris.Service.Boot
import           Boris.Service.Listener
import           Boris.Service.Guard (TerminationAction (..), TerminationHandler (..), repeatedly)
import           Boris.Service.Snooze (seconds)
import           Boris.Prelude

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.IO (IO)

import           Boris.Git.Pin (Pin, checkPin)



run :: LogService -> BuildService -> DiscoverService -> Boris -> WorkspacePath -> Pin -> IO ()
run logs builds discovers boris work pin = do
  repeatedly {-- TODO (seconds 1) --} (seconds 10) (handler "listener" renderListenerError) $
    let go = listen logs builds discovers boris work in liftIO (checkPin pin) >>= flip unless go

handler :: Text -> (e -> Text) -> TerminationHandler e
handler label render =
  TerminationHandler {
      onExplosion = \e -> do
        Die <$ (T.putStrLn . mconcat)
--TODO        Restart <$ (T.putStrLn . mconcat)
          ["The supervised thread ", label, " unexpectedly exploded: ", T.pack . show $ e]
    , onError = \e ->
        Die <$ (T.putStrLn . mconcat)
--TODO       Restart <$ (T.putStrLn . mconcat)
          ["The supervised thread ", label, " unexpectedly errored: ", render e]
    , onGraceful =
       Die <$ (T.putStrLn . mconcat)
          ["The supervised thread ", label, " has been stopped gracefully."]
    }
