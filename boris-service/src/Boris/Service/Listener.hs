{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Listener (
    ListenerError (..)
  , listen
  , renderListenerError
  ) where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Workspace
--import           Boris.Queue (BuildQueue (..), Request (..), RequestBuild (..), RequestDiscover (..))
--import qualified Boris.Queue as Q
import           Boris.Service.Boot
import           Boris.Service.Build
import           Boris.Service.Discover
import           Boris.Prelude

import           System.IO (IO)

data ListenerError =
    ListenerBuilderError BuilderError
  | ListenerDiscoverError DiscoverError

listen :: LogService -> BuildService -> DiscoverService -> WorkspacePath -> EitherT ListenerError IO ()
listen logs builds discovers w = do
  error "todo"
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
