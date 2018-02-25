{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Build (
    byId
  , list
  , queued
  , submit
  , heartbeat
  , acknowledge
  , cancel
  , byCommit
  , byProject
  , logOf
  , avow
  , complete
  , BuildError (..)
  , renderBuildError
  ) where


import           Control.Monad.IO.Class (MonadIO (..))

import qualified Data.Text as Text
import qualified Data.Time as Time

import           Boris.Core.Data
import qualified Boris.Http.Api.Project as Project
import           Boris.Http.Boot

import qualified Boris.Http.Service as Service
import qualified Boris.Http.Store.Api as Store
import           Boris.Http.Store.Data
import qualified Boris.Http.Store.Error as Store
import           Boris.Queue (Request (..), RequestBuild (..))

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)

data BuildError =
    BuildStoreError Store.StoreError
  | BuildRegisterError Store.RegisterError
  | BuildConfigError Project.ConfigError
  | BuildServiceError Service.ServiceError

renderBuildError :: BuildError -> Text
renderBuildError err =
  case err of
    BuildStoreError e ->
      mconcat ["Build error via store backend: ", Store.renderStoreError e]
    BuildRegisterError e ->
      mconcat ["Build registration error via store backend: ", Store.renderRegisterError e]
    BuildConfigError e ->
      mconcat ["Build project configuration error: ", Project.renderConfigError e]
    BuildServiceError e ->
      mconcat ["Build service error: ", Service.renderServiceError e]

byId :: Store -> BuildId -> EitherT Store.FetchError IO (Maybe BuildData)
byId store build = do
  result <- Store.fetch store build
  for result $ \r ->
    case buildDataResult r of
      Nothing ->
        case buildDataHeartbeatTime r of
          Nothing -> do
            case buildDataCancelled r of
              Nothing ->
                pure r
              Just BuildNotCancelled ->
                pure r
              Just BuildCancelled ->
                pure $ r { buildDataResult = Just . fromMaybe BuildKo . buildDataResult $ r }
          Just h -> do
            now <- liftIO Time.getCurrentTime
            if Time.diffUTCTime now h > 120
              then do
                firstT Store.FetchBackendError $
                  Store.cancelx store build
                pure $ r { buildDataResult = Just . fromMaybe BuildKo . buildDataResult $ r }
              else
                pure r
      Just _ ->
        pure r

list :: Store -> Project -> Build -> EitherT Store.StoreError IO BuildTree
list store project build = do
  refs <- Store.getBuildRefs store project build
  BuildTree project build <$> (for refs $ \ref ->
    BuildTreeRef ref <$> Store.getBuildIds store project build ref)

queued :: Store -> Project -> Build -> EitherT Store.StoreError IO [BuildId]
queued store project build =
  Store.getQueued store project build

submit :: Store -> BuildService -> ProjectMode -> Project -> Build -> Maybe Ref -> EitherT BuildError IO (Maybe BuildId)
submit store buildx projectx project build ref = do
  repository' <- firstT BuildConfigError $
    Project.pick projectx project
  case repository' of
    Nothing ->
      pure Nothing
    Just repository -> do
      i <- firstT BuildStoreError $
        Store.tick store
      firstT BuildRegisterError $
        Store.register store project build i
      let
        normalised = with ref $ \rr ->
          if Text.isPrefixOf "refs/" . renderRef $ rr then rr else Ref . ((<>) "refs/heads/") . renderRef $ rr
        req = RequestBuild' $ RequestBuild i project repository build normalised
      firstT BuildServiceError $
        Service.put buildx req
      pure $ Just i

heartbeat :: Store -> BuildId -> EitherT Store.StoreError IO BuildCancelled
heartbeat store buildId =
  Store.heartbeat store buildId

acknowledge :: Store -> BuildId -> EitherT Store.StoreError IO Acknowledge
acknowledge store buildId =
  Store.acknowledge store buildId

cancel :: Store -> BuildId -> EitherT Store.FetchError IO (Maybe ())
cancel store i = do
  d <- Store.fetch store i
  for d $ \_ ->
    firstT Store.FetchBackendError $ do
      Store.cancel store i

byCommit :: Store -> Project -> Commit -> EitherT Store.StoreError IO [BuildId]
byCommit store project commit =
  Store.getProjectCommitBuildIds store project commit

byProject :: Store -> Project -> EitherT Store.StoreError IO [Build]
byProject store project =
  Store.getProjects store project

logOf :: Store -> LogService -> BuildId -> EitherT Store.FetchError IO (Maybe [Text])
logOf store logs i = do
  d <- Store.fetch store i
  case d of
    Nothing ->
      pure Nothing
    Just _ -> do
      case logs of
        DBLogs -> do
          l' <- Store.fetchLogData store i
          case l' of
            Just (DBLog l) ->
              pure $ Just (renderDBLogData <$> l)
            Nothing ->
              pure Nothing
        DevNull ->
          pure Nothing

avow :: Store -> BuildId -> Ref -> Commit -> EitherT Store.StoreError IO ()
avow store i ref commit = do
  Store.index store i ref commit

complete :: Store -> BuildId -> BuildResult -> EitherT Store.StoreError IO ()
complete store i result = do
  Store.complete store i result
