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
import           Boris.Store.Tick (TickError, renderTickError)
import qualified Boris.Store.Tick as ST
import           Boris.Queue (BuildQueue (..), Request (..), RequestBuild (..), RequestDiscover (..))
import qualified Boris.Queue as Q
import qualified Boris.X as X

import           Control.Monad.Trans.Class (lift)

import qualified Data.List as L

import           Jebediah.Data (GroupName (..), StreamName (..))

import           Mismi (Error, runAWST, renderError)
import           Mismi.Amazonka (Env)

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, bimapEitherT)

data DiscoverError =
    DiscoverAwsError Error
  | DiscoverInitialiseError InitialiseError
  | DiscoverTickError TickError

discover :: Env -> Environment -> BuildQueue -> WorkspacePath -> RequestDiscover -> EitherT DiscoverError IO ()
discover env e q w request = do
  let
    buildId = requestDiscoverId request
    project = requestDiscoverProject request
    repository = requestDiscoverRepository request
    gname = GroupName $ mconcat ["boris.discover.", renderEnvironment e]
    sname = StreamName $ mconcat [renderProject project, ".", renderBuildId buildId]

  out <- runAWST env DiscoverAwsError . lift $
    newLogger gname sname

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

      if L.elem build current
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
          newId <- runAWST env DiscoverAwsError . bimapEitherT DiscoverTickError id $
            ST.next e project build
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
