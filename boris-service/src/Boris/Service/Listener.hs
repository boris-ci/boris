{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Listener (
    ListenerError (..)
  , listen
  , renderListenerError
  ) where

import           Boris.Core.Data
import           Boris.Service.Boot
import           Boris.Service.Build
import           Boris.Service.Discover
import           Boris.Queue (BuildQueue (..), Request (..), RequestBuild (..), RequestDiscover (..))
import qualified Boris.Queue as Q

import           Mismi (Error, runAWST, renderError)
import           Mismi.Amazonka (Env)

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, bimapEitherT)

data ListenerError =
    ListenerQueueError Q.QueueError
  | ListenerAwsError Error
  | ListenerBuilderError BuilderError
  | ListenerDiscoverError DiscoverError

listen :: LogService -> BuildService -> DiscoverService -> Env -> BuildQueue -> WorkspacePath -> EitherT ListenerError IO ()
listen logs builds discovers env q w = do
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

renderListenerError :: ListenerError -> Text
renderListenerError err =
  case err of
    ListenerQueueError e ->
      mconcat ["An error occurred trying to obtain a build to run: ", Q.renderQueueError e]
    ListenerAwsError e ->
      mconcat ["An AWS error occurred trying to obtain a build to run: ", renderError e]
    ListenerDiscoverError e ->
      mconcat ["An error occurred during a discovery: ", renderDiscoverError e]
    ListenerBuilderError e ->
      mconcat ["An error occurred during a build: ", renderBuilderError e]
