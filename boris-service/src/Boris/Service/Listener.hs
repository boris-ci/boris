{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Listener (
    ListenerError (..)
  , listen
  , renderListenerError
  ) where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Workspace
import           Boris.Client.Error
import           Boris.Client.Config (Boris)
import qualified Boris.Client.Build as Build
import qualified Boris.Client.Network as Network
import           Boris.Service.Boot
import           Boris.Service.Build
import           Boris.Service.Discover
import           Boris.Prelude

import           Control.Monad.IO.Class (MonadIO (..))

import           System.IO (IO)
import qualified System.IO as IO

data ListenerError =
    ListenerBuilderError BuilderError
  | ListenerDiscoverError DiscoverError
  | ListenerNetworkError BorisError

listen :: LogService -> BuildService -> DiscoverService -> Boris -> WorkspacePath -> EitherT ListenerError IO ()
listen logs builds discovers boris w = do
  candidate <- firstT ListenerNetworkError . Network.runRequestT boris $
    Build.next

  case candidate of
    Nothing ->
      liftIO $ IO.putStrLn "no builds found on queue."
    Just build ->
      firstT ListenerBuilderError $
        builder logs builds w build

  {--
  r <- runAWST env ListenerAwsError . bimapEitherT ListenerQueueError id $
    Q.get q

  forM_ r $ \request -> do
    case request of
      RequestDiscover' x ->
        bimapEitherT ListenerDiscoverError id $
          discover logs discovers w (requestDiscoverId x) (requestDiscoverProject x) (requestDiscoverRepository x)
      RequestBuild' x ->
        bimapEitherT ListenerBuilderError id $
          builder logs builds w (requestBuildId x) (requestBuildProject x) (requestBuildRepository x) (requestBuildName x) (requestBuildRef x)
--}

renderListenerError :: ListenerError -> Text
renderListenerError err =
  case err of
    ListenerDiscoverError e ->
      mconcat ["An error occurred during a discovery: ", renderDiscoverError e]
    ListenerBuilderError e ->
      mconcat ["An error occurred during a build: ", renderBuilderError e]
    ListenerNetworkError e ->
      mconcat ["An error occurred talking to service: ", renderBorisError e]
