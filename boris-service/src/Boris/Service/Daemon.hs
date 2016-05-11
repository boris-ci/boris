{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Daemon (
   run
  ) where

import           Boris.Core.Data
import           Boris.Service.Listener
import           Boris.Queue (BuildQueue (..))

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           P

import           Mismi.Amazonka (Env)

import           System.IO (IO)

import           Twine.Data.Pin (Pin, checkPin)
import           Twine.Snooze (seconds)
import           Twine.Guard (TerminationAction (..), TerminationHandler (..), repeatedly)


run :: Env -> Environment -> BuildQueue -> WorkspacePath -> Pin -> IO ()
run env e q work pin = do
  repeatedly (seconds 1) (handler "listener" renderListenerError) $
    let go = listen env e q work in liftIO (checkPin pin) >>= flip unless go

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
