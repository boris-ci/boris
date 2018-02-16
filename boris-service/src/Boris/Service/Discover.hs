{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Boris.Service.Discover (
    DiscoverError (..)
  , discover
  , renderDiscoverError
  ) where

import qualified Boris.Client.Discover as Discover
import qualified Boris.Client.Http as Http
import           Boris.Core.Data
import           Boris.Service.Boot
import           Boris.Service.Git
import           Boris.Service.Log
import           Boris.Service.Workspace

import           P

import           System.IO (IO)

import qualified Tine.Conduit as X

import           X.Control.Monad.Trans.Either (EitherT, bimapEitherT, joinEitherE, newEitherT, runEitherT)

data DiscoverError =
    DiscoverInitialiseError InitialiseError
  | DiscoverLogError LogError
  | DiscoverHttpError Http.BorisHttpClientError

discover :: LogService -> DiscoverService ->  WorkspacePath -> BuildId -> Project -> Repository -> EitherT DiscoverError IO ()
discover logs discovers w buildid project repository = do
  joinEitherE join . newEitherT . firstT DiscoverLogError . withLogger logs project buildid  $ \out -> runEitherT $
    withWorkspace w buildid $ \workspace -> do
      X.xPutStrLn out . mconcat $ ["[boris:discover] ", renderProject project]

      discovered <- bimapEitherT DiscoverInitialiseError id $
        discovering out out workspace repository

      case discovers of
        PushDiscover config -> do
          firstT DiscoverHttpError $
            Discover.complete config buildid project discovered
        LogDiscover -> do
           X.xPutStrLn out . mconcat $ ["project = ", renderProject project]
           X.xPutStrLn out . mconcat $ ["repository = ", renderRepository repository]
           for_ discovered $ \(DiscoverInstance b r c) -> do
             X.xPutStrLn out . mconcat $ ["  build = ", renderBuild b]
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
      mconcat ["A http error has occurred trying to discover builds: ", Http.renderBorisHttpClientError e]
