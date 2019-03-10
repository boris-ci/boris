{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Boris.Service.Discover (
    DiscoverError (..)
  , discover
  , renderDiscoverError
  ) where

import           Boris.Client.Error
import qualified Boris.Client.Discover as Discover
import qualified Boris.Client.Network as Network
import           Boris.Core.Data.Build
import           Boris.Core.Data.Discover
import           Boris.Core.Data.Instance
import           Boris.Core.Data.Keyed
import           Boris.Core.Data.Project
import           Boris.Core.Data.Repository
import           Boris.Core.Data.Workspace
import qualified Boris.Git.X as X
import           Boris.Service.Boot
import           Boris.Service.Git
import           Boris.Service.Log
import           Boris.Service.Workspace
import           Boris.Prelude

import           System.IO (IO)

data DiscoverError =
    DiscoverInitialiseError InitialiseError
  | DiscoverLogError LogError
  | DiscoverHttpError BorisError

discover :: LogService -> DiscoverService ->  WorkspacePath -> (Keyed DiscoverId Discover) -> EitherT DiscoverError IO ()
discover logs discovers w discover = do
  mapEitherT (fmap join) . firstT DiscoverLogError . withLogger logs $ \out -> runEitherT $
    withWorkspace w (BuildId . getDiscoverId . keyOf $ discover) $ \workspace -> do
      let
        discoverId = keyOf discover
        project = projectName . valueOf . discoverProject . valueOf $ discover
        repository = projectRepository . valueOf . discoverProject . valueOf $ discover

      X.xPutStrLn out . mconcat $ ["[boris:discover] ", renderProjectName project]

      discovered <- bimapEitherT DiscoverInitialiseError id $
        discovering out out workspace repository

      case discovers of
        PushDiscover http -> do
          firstT DiscoverHttpError $
            Network.runRequestT http $
              Discover.complete discoverId project discovered
        LogDiscover -> do
           X.xPutStrLn out . mconcat $ ["project = ", renderProjectName project]
           X.xPutStrLn out . mconcat $ ["repository = ", renderRepository repository]
           for_ discovered $ \(DiscoverInstance b r c) -> do
             X.xPutStrLn out . mconcat $ ["  build = ", renderBuildName b]
             X.xPutStrLn out . mconcat $ ["  ref = ", renderRef r]
             X.xPutStrLn out . mconcat $ ["  commit = ", renderCommit c]
             X.xPutStrLn out . mconcat $ [""]

renderDiscoverError :: DiscoverError -> Text
renderDiscoverError err =
  case err of
    DiscoverInitialiseError e ->
      mconcat ["A git initialisation error has occurred trying to discover builds: ", renderInitialiseError e]
    DiscoverLogError e ->
      mconcat ["A logging error has occurred trying to discover builds: ", renderLogError e]
    DiscoverHttpError e ->
      mconcat ["A http error has occurred trying to discover builds: ", renderBorisError e]
