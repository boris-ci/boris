{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Discover (
    DiscoverError (..)
  , discover
  , renderDiscoverError
  ) where

import           Boris.Core.Data
import           Boris.Service.Git
import           Boris.Service.Log
import           Boris.Service.Workspace
import qualified Boris.Store.Index as SI
import           Boris.Store.Build (RegisterError, renderRegisterError)
import qualified Boris.Store.Build as SB
import           Boris.Store.Tick (TickError, renderTickError)
import qualified Boris.Store.Tick as ST
import           Boris.Queue (BuildQueue (..), Request (..), RequestBuild (..), RequestDiscover (..))
import qualified Boris.Queue as Q

import           Control.Monad.Trans.Class (lift)

import qualified Data.List as L

import           Jebediah.Data (LogGroup (..), LogStream (..))

import           Mismi (Error, runAWST, renderError)
import           Mismi.Amazonka (Env)

import           P

import           System.IO (IO)

import qualified Tine.Conduit as X

import           X.Control.Monad.Trans.Either (EitherT, runEitherT, newEitherT, bimapEitherT)

data DiscoverError =
    DiscoverAwsError Error
  | DiscoverInitialiseError InitialiseError
  | DiscoverTickError TickError
  | DiscoverRegisterError RegisterError

discover :: Env -> Environment -> BuildQueue -> WorkspacePath -> RequestDiscover -> EitherT DiscoverError IO ()
discover env e q w request = do
  let
    buildId = requestDiscoverId request
    project = requestDiscoverProject request
    repository = requestDiscoverRepository request
    gname = LogGroup $ mconcat ["boris.discover.", renderEnvironment e]
    sname = LogStream $ mconcat [renderProject project, ".", renderBuildId buildId]

  runAWST env DiscoverAwsError . newEitherT . withLogger gname sname $ \out -> runEitherT $
    withWorkspace w buildId $ \workspace -> do
      X.xPutStrLn out . mconcat $ ["[boris:discover] ", renderProject project]

      discovered <- bimapEitherT DiscoverInitialiseError id $
        discovering out out workspace repository

      forM_ discovered $ \d -> do
        let
          build = buildName . discoverBuildPattern $ d
          commit = discoverCommit d
          ref = discoverRef d

        current <- runAWST env DiscoverAwsError . lift $
          SI.getProjectCommitSeen e project commit

        already <- runAWST env DiscoverAwsError . lift $
          SI.getProjectCommitDiscovered e project commit

        if L.elem build current || L.elem build already
          then do
            X.xPutStrLn out $ mconcat [
                "Already seen"
              , ": project = ", renderProject project
              , ", build = ", renderBuild build
              , ", ref = ", renderRef ref
              , ", commit = ", renderCommit commit
              ]
            pure ()
          else do
            runAWST env DiscoverAwsError . lift $
              SI.addProjectCommitDiscovered e project commit build
            newId <- runAWST env DiscoverAwsError . bimapEitherT DiscoverTickError id $
              ST.next e project build
            _ <- runAWST env DiscoverAwsError . bimapEitherT DiscoverRegisterError id $
              SB.register e project build newId
            X.xPutStrLn out $ mconcat [
                "New commit, triggering build"
              , ": project = ", renderProject project
              , ", build = ", renderBuild build
              , ", ref = ", renderRef ref
              , ", commit = ", renderCommit commit
              , ", build-id ", renderBuildId newId
              ]
            runAWST env DiscoverAwsError . lift $
              Q.put q (RequestBuild' $ RequestBuild newId project repository build (Just ref))

renderDiscoverError :: DiscoverError -> Text
renderDiscoverError err =
  case err of
    DiscoverAwsError e ->
      mconcat ["An AWS error occurred trying to discover builds: ", renderError e]
    DiscoverInitialiseError e ->
      mconcat ["A git initialisation error has occurred trying to discover builds: ", renderInitialiseError e]
    DiscoverTickError e ->
      mconcat ["An error has occurred trying to generate an id for a discovered build: ", renderTickError e]
    DiscoverRegisterError e ->
      mconcat ["An error has occurred trying to register an id for a discovered build: ", renderRegisterError e]
