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
import qualified Boris.Http.Db.Query as Query

import qualified Boris.Http.Service as Service
import           Boris.Queue (Request (..), RequestBuild (..))

import           P

import           System.IO (IO)

import           Traction.Control (DbPool, DbError)
import qualified Traction.Control as Traction

import           X.Control.Monad.Trans.Either (EitherT)

data BuildError =
    BuildDbError Traction.DbError
  | BuildConfigError Project.ConfigError
  | BuildServiceError Service.ServiceError

renderBuildError :: BuildError -> Text
renderBuildError err =
  case err of
    BuildDbError e ->
      mconcat ["Build error via db: ", Traction.renderDbError e]
    BuildConfigError e ->
      mconcat ["Build project configuration error: ", Project.renderConfigError e]
    BuildServiceError e ->
      mconcat ["Build service error: ", Service.renderServiceError e]

byId :: DbPool -> BuildId -> EitherT DbError IO (Maybe BuildData)
byId pool build =
  Traction.runDb pool $ do
    result <- Query.fetch build
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
                  Query.cancel build
                  pure $ r { buildDataResult = Just . fromMaybe BuildKo . buildDataResult $ r }
                else
                  pure r
        Just _ ->
          pure r

list :: DbPool -> Project -> Build -> EitherT DbError IO BuildTree
list pool project build = Traction.runDb pool $ do
  refs <- Query.getBuildRefs project build
  BuildTree project build <$> (for refs $ \ref ->
    BuildTreeRef ref <$> Query.getBuildIds project build ref)

queued :: DbPool -> Project -> Build -> EitherT DbError IO [BuildId]
queued pool project build =
  Traction.runDb pool $
  Query.getQueued project build

submit :: DbPool -> BuildService -> ProjectMode -> Project -> Build -> Maybe Ref -> EitherT BuildError IO (Maybe BuildId)
submit pool buildx projectx project build ref = do
  repository' <- firstT BuildConfigError $
    Project.pick projectx project
  case repository' of
    Nothing ->
      pure Nothing
    Just repository -> do
      i <- firstT BuildDbError . Traction.runDb pool $ do
        i <- Query.tick
        Query.register project build i
        pure i
      let
        normalised = with ref $ \rr ->
          if Text.isPrefixOf "refs/" . renderRef $ rr then rr else Ref . ((<>) "refs/heads/") . renderRef $ rr
        req = RequestBuild' $ RequestBuild i project repository build normalised
      firstT BuildServiceError $
        Service.put buildx req
      pure $ Just i

heartbeat :: DbPool -> BuildId -> EitherT DbError IO BuildCancelled
heartbeat pool  buildId =
  Traction.runDb pool $
    Query.heartbeat buildId

acknowledge :: DbPool -> BuildId -> EitherT DbError IO Acknowledge
acknowledge pool buildId =
  Traction.runDb pool $
    Query.acknowledge' buildId

cancel :: DbPool -> BuildId -> EitherT DbError IO (Maybe ())
cancel pool i =
  Traction.runDb pool $ do
    d <- Query.fetch i
    for d $ \_ ->
        Query.cancel i

byCommit :: DbPool -> Project -> Commit -> EitherT DbError IO [BuildId]
byCommit pool project commit =
  Traction.runDb pool $
    Query.getProjectCommitBuildIds project commit

byProject :: DbPool -> Project -> EitherT DbError IO [Build]
byProject pool project =
  Traction.runDb pool $
    Query.getProjects project

logOf :: DbPool -> LogService -> BuildId -> EitherT DbError IO (Maybe LogData)
logOf pool logs i =
  Traction.runDb pool $ do
    d <- Query.fetch pool
    case d of
      Nothing ->
        pure Nothing
      Just _ -> do
        case logs of
          DBLogs -> do
            l' <- Query.fetchLogData i
            case l' of
              Just dbl ->
                pure $ Just dbl
              Nothing ->
                pure Nothing
          DevNull ->
            pure Nothing

avow :: DbPool -> BuildId -> Ref -> Commit -> EitherT DbError IO ()
avow pool i ref commit =
  Traction.runDb pool $
    Query.index i ref commit

complete :: DbPool -> BuildId -> BuildResult -> EitherT DbError IO ()
complete pool i result =
  void . Traction.runDb pool $
    Query.complete i result
