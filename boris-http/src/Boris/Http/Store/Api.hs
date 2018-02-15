{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Store.Api (
    initialise
  , tick
  , getProjectCommitBuildIds
  , getProjectCommitSeen
  , getProjectCommitDiscovered
  , addProjectCommitDiscovered
  , getProjects
  , getBuildRefs
  , getBuildIds
  , getQueued
  , fetch
  , results
  , register
  , cancel
  , cancelx
  , heartbeat
  , acknowledge
  , index
  , deindex
  , complete
  , discover
  , userByGithubId
  , updateUser
  , addUser
  , newSession
  , tickSession
  , getSessionUser
  , getSessionOAuth
  ) where

import           Boris.Core.Data
import           Boris.Http.Data
import           Boris.Http.Store.Data
import           Boris.Http.Store.Error
import qualified Boris.Http.Store.Dynamo.Build as Dynamo
import qualified Boris.Http.Store.Dynamo.Index as Dynamo
import qualified Boris.Http.Store.Dynamo.Lifecycle as Dynamo
import qualified Boris.Http.Store.Dynamo.Tick as Dynamo
import qualified Boris.Http.Store.Dynamo.Results as Dynamo

import qualified Boris.Http.Store.Postgres.Schema as Postgres
import qualified Boris.Http.Store.Postgres.Query as Postgres

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Class (MonadTrans (..))

import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Time as Time
import qualified Data.Text as Text

import qualified Mismi.Control as Mismi

import           P

import           System.IO (IO)

import qualified Traction.Control as Traction
import qualified Traction.Migration as Traction

import           X.Control.Monad.Trans.Either (EitherT, left)


initialise :: Store -> EitherT StoreError IO ()
initialise s =
  case s of
    DynamoStore env e ->
      Mismi.runAWST env DynamoBackendError . firstT InitialisationError $
        Dynamo.initialise e
    PostgresStore pool ->
      void . firstT PostgresBackendError . Traction.runDb pool $
        Traction.migrate Postgres.schema
    MemoryStore _ ->
      pure ()

tick :: Store -> EitherT StoreError IO BuildId
tick = \case
  DynamoStore env e ->
    Mismi.runAWST env DynamoBackendError . firstT TickError $
      Dynamo.next e
  PostgresStore pool ->
    firstT PostgresBackendError . Traction.runDb pool $
      Postgres.tick
  MemoryStore ref ->
    liftIO $ IORef.atomicModifyIORef' ref $ \(n, builds, discovers) ->
      ((n + 1, builds, discovers), BuildId . Text.pack . show $ n)


getProjectCommitBuildIds :: Store -> Project -> Commit -> EitherT StoreError IO [BuildId]
getProjectCommitBuildIds store project commit =
  case store of
    DynamoStore env e ->
      firstT DynamoBackendError . Mismi.runAWS env $
        Dynamo.getProjectCommitBuildIds e project commit
    PostgresStore pool ->
      firstT PostgresBackendError . Traction.runDb pool $
        Postgres.getProjectCommitBuildIds project commit
    MemoryStore ref ->
      liftIO $ do
        (_, builds, _) <- IORef.readIORef ref
        pure . List.nub . List.sort . fmap buildDataId $ builds

getProjectCommitSeen :: Store -> Project -> Commit -> EitherT StoreError IO [Build]
getProjectCommitSeen store project commit =
  case store of
    DynamoStore env e ->
      firstT DynamoBackendError . Mismi.runAWS env $
        Dynamo.getProjectCommitSeen e project commit
    PostgresStore pool ->
      firstT PostgresBackendError . Traction.runDb pool $
        Postgres.getProjectCommitSeen project commit
    MemoryStore ref ->
      liftIO $ IORef.atomicModifyIORef' ref $ \(n, builds, discovers) ->
        ((n, builds, discovers), List.nub . fmap buildDataBuild . filter (\d ->
          buildDataProject d == project && buildDataCommit d == Just commit) $ builds)

getProjectCommitDiscovered :: Store -> Project -> Commit -> EitherT StoreError IO [Build]
getProjectCommitDiscovered store project commit =
  case store of
    DynamoStore env e ->
      firstT DynamoBackendError . Mismi.runAWS env $
        Dynamo.getProjectCommitDiscovered e project commit
    PostgresStore pool ->
      firstT PostgresBackendError . Traction.runDb pool $
        Postgres.getProjectCommitDiscovered project commit
    MemoryStore ref ->
      liftIO $ IORef.atomicModifyIORef' ref $ \(n, builds, discovers) ->
        ((n, builds, discovers), List.nub . fmap discoverBuild . filter ((==) commit . discoverCommit) . fmap snd . filter ((==) project . fst) $ discovers)

addProjectCommitDiscovered :: Store -> BuildId -> Project -> Commit -> Ref -> Build -> EitherT StoreError IO ()
addProjectCommitDiscovered store buildId project commit ref build =
  case store of
    DynamoStore env e ->
      firstT DynamoBackendError . Mismi.runAWS env $
        Dynamo.addProjectCommitDiscovered e project commit build
    PostgresStore pool ->
      firstT PostgresBackendError . Traction.runDb pool $
        Postgres.addProjectCommitDiscovered buildId build commit
    MemoryStore refx ->
      liftIO $ IORef.atomicModifyIORef' refx $ \(n, builds, discovers) ->
        ((n, builds, (project, DiscoverInstance build ref commit) : discovers), ())

getProjects :: Store -> Project -> EitherT StoreError IO [Build]
getProjects store project =
  case store of
    DynamoStore env e ->
      firstT DynamoBackendError . Mismi.runAWS env $
        Dynamo.getProjects e project
    PostgresStore pool ->
      firstT PostgresBackendError . Traction.runDb pool $
        Postgres.getProjects project
    MemoryStore refx ->
      liftIO $ IORef.atomicModifyIORef' refx $ \(n, builds, discovers) ->
        ((n, builds, discovers), List.nub . fmap buildDataBuild . filter ((==) project . buildDataProject) $ builds)

getBuildRefs :: Store -> Project -> Build -> EitherT StoreError IO [Ref]
getBuildRefs store project build =
  case store of
    DynamoStore env e ->
      firstT DynamoBackendError . Mismi.runAWS env $
        Dynamo.getBuildRefs e project build
    PostgresStore pool ->
      firstT PostgresBackendError . Traction.runDb pool $
        Postgres.getBuildRefs project build
    MemoryStore refx ->
      liftIO $ IORef.atomicModifyIORef' refx $ \(n, builds, discovers) ->
        ((n, builds, discovers), List.nub . catMaybes . fmap buildDataRef . filter ((==) build . buildDataBuild) . filter ((==) project . buildDataProject) . filter (isJust . buildDataRef) $ builds)

getBuildIds :: Store -> Project -> Build -> Ref -> EitherT StoreError IO [BuildId]
getBuildIds store project build ref =
  case store of
    DynamoStore env e ->
      firstT DynamoBackendError . Mismi.runAWS env $
        Dynamo.getBuildIds e project build ref
    PostgresStore pool ->
      firstT PostgresBackendError . Traction.runDb pool $
        Postgres.getBuildIds project build ref
    MemoryStore refx ->
      liftIO $ IORef.atomicModifyIORef' refx $ \(n, builds, discovers) ->
        ((n, builds, discovers), List.nub . fmap buildDataId . filter ((==) build . buildDataBuild) . filter ((==) project . buildDataProject) . filter ((==) (Just ref) . buildDataRef) $ builds)

getQueued :: Store -> Project -> Build -> EitherT StoreError IO [BuildId]
getQueued store project build =
  case store of
    DynamoStore env e ->
      firstT DynamoBackendError . Mismi.runAWS env $
        Dynamo.getQueued e project build
    PostgresStore pool ->
      firstT PostgresBackendError . Traction.runDb pool $
        Postgres.getQueued  project build
    MemoryStore refx ->
      liftIO $ IORef.atomicModifyIORef' refx $ \(n, builds, discovers) ->
        ((n, builds, discovers), List.nub . fmap buildDataId . filter ((==) build . buildDataBuild) . filter ((==) project . buildDataProject) . filter (isNothing . buildDataRef) . filter (isNothing . buildDataResult) $ builds)

fetch :: Store -> BuildId -> EitherT FetchError IO (Maybe BuildData)
fetch store build =
  case store of
    DynamoStore env e ->
      fmap Just . Mismi.runAWST env (FetchBackendError . DynamoBackendError) $
        Dynamo.fetch e build
    PostgresStore pool ->
      firstT (FetchBackendError . PostgresBackendError) . Traction.runDb pool $
        Postgres.fetch build
    MemoryStore refx ->
      liftIO $ IORef.atomicModifyIORef' refx $ \(n, builds, discovers) ->
        ((n, builds, discovers), head . filter ((==) build . buildDataId) $ builds)

register :: Store -> Project -> Build -> BuildId -> EitherT RegisterError IO ()
register store project build buildid =
  case store of
    DynamoStore env e ->
      Mismi.runAWST env (RegisterStoreError . DynamoBackendError) $
        Dynamo.register e project build buildid
    PostgresStore pool ->
      firstT (RegisterStoreError . PostgresBackendError) . Traction.runDb pool $
        Postgres.register project build buildid
    MemoryStore refx -> do
      now <- liftIO $ Time.getCurrentTime
      liftIO $ IORef.atomicModifyIORef' refx $ \(n, builds, discovers) ->
        ((n, (BuildData buildid project build Nothing Nothing (Just now) Nothing Nothing Nothing Nothing Nothing Nothing) : builds, discovers), ())


index :: Store -> BuildId -> Project -> Build -> Ref -> Commit -> EitherT StoreError IO ()
index store buildId project build ref commit  =
  case store of
    DynamoStore env e ->
      Mismi.runAWST env DynamoBackendError . lift $
        Dynamo.index e project build buildId ref commit
    PostgresStore pool ->
      firstT PostgresBackendError . Traction.runDb pool $
        Postgres.index buildId ref commit
    MemoryStore refx -> do
      let
        update x =
          case buildDataId x == buildId of
            True ->
              x { buildDataRef = Just ref, buildDataCommit = Just commit}
            False ->
              x
      liftIO $ IORef.atomicModifyIORef' refx $ \(n, builds, discovers) ->
        ((n, update <$> builds, discovers), ())

deindex :: Store -> BuildId -> Project -> Build -> EitherT StoreError IO ()
deindex store buildId project build =
  case store of
    DynamoStore env e ->
      Mismi.runAWST env DynamoBackendError . lift $
        Dynamo.deindex e project build buildId
    PostgresStore _pool ->
      pure ()
    MemoryStore _ref ->
      pure ()

cancel :: Store -> Project -> Build -> BuildId -> EitherT StoreError IO ()
cancel store project build buildid =
  case store of
    DynamoStore env e ->
      void . firstT DynamoBackendError . Mismi.runAWS env $ do
        Dynamo.deindex e project build buildid
        Dynamo.cancel e buildid
    PostgresStore pool ->
      firstT (PostgresBackendError) . Traction.runDb pool $
        Postgres.cancel buildid
    MemoryStore refx -> do
      now <- liftIO $ Time.getCurrentTime
      let
        update x =
          case buildDataId x == buildid && isNothing (buildDataCancelled x) of
            True ->
              x { buildDataCancelled = Just BuildCancelled, buildDataResult = Just BuildKo, buildDataEndTime = Just now}
            False ->
              x
      liftIO $ IORef.atomicModifyIORef' refx $ \(n, builds, discovers) ->
        ((n, update <$> builds, discovers), ())

cancelx :: Store -> BuildId -> EitherT StoreError IO ()
cancelx store buildid =
  case store of
    DynamoStore env e ->
      void . firstT DynamoBackendError . Mismi.runAWS env $ do
        Dynamo.cancel e buildid
    PostgresStore pool ->
      firstT (PostgresBackendError) . Traction.runDb pool $
        Postgres.cancel buildid
    MemoryStore refx -> do
      now <- liftIO $ Time.getCurrentTime
      let
        update x =
          case buildDataId x == buildid && isNothing (buildDataCancelled x) of
            True ->
              x { buildDataCancelled = Just BuildCancelled, buildDataResult = Just BuildKo, buildDataEndTime = Just now}
            False ->
              x
      liftIO $ IORef.atomicModifyIORef' refx $ \(n, builds, discovers) ->
        ((n, update <$> builds, discovers), ())

heartbeat :: Store -> BuildId -> EitherT StoreError IO BuildCancelled
heartbeat store buildid =
  case store of
    DynamoStore env e ->
      firstT DynamoBackendError . Mismi.runAWS env $ do
        Dynamo.heartbeat e buildid
    PostgresStore pool ->
      firstT (PostgresBackendError) . Traction.runDb pool $
        Postgres.heartbeat buildid
    MemoryStore refx -> do
      now <- liftIO $ Time.getCurrentTime
      let
        update x =
          case buildDataId x == buildid of
            True ->
              x { buildDataHeartbeatTime = Just now}
            False ->
              x
      liftIO $ IORef.atomicModifyIORef' refx $ \(n, builds, discovers) ->
        ((n, update <$> builds, discovers), fromMaybe BuildNotCancelled $
          join . head . fmap buildDataCancelled . filter ((==) buildid . buildDataId) $ builds)

acknowledge :: Store -> BuildId -> EitherT StoreError IO Acknowledge
acknowledge store buildid =
  case store of
    DynamoStore env e ->
      firstT DynamoBackendError . Mismi.runAWS env $ do
        Dynamo.acknowledge' e buildid
    PostgresStore pool ->
      firstT (PostgresBackendError) . Traction.runDb pool $
        Postgres.acknowledge' buildid
    MemoryStore refx -> do
      now <- liftIO $ Time.getCurrentTime
      let
        update x =
          case buildDataId x == buildid && isNothing (buildDataStartTime x)of
            True ->
              x { buildDataStartTime = Just now}
            False ->
              x
      liftIO $ IORef.atomicModifyIORef' refx $ \(n, builds, discovers) ->
        ((n, update <$> builds, discovers), maybe Accept (const AlreadyRunning) $
          head . filter (isNothing . buildDataStartTime) . filter ((==) buildid . buildDataId) $ builds)

results :: Store -> EitherT StoreError IO [Result]
results store =
  case store of
    DynamoStore env e ->
      Mismi.runAWST env DynamoBackendError $
        firstT (ResultsError . Dynamo.jsonError) $
          Dynamo.compress e
    PostgresStore pool ->
      firstT (PostgresBackendError) . Traction.runDb pool $
        Postgres.results
    MemoryStore refx ->
      liftIO $ IORef.atomicModifyIORef' refx $ \(n, builds, discovers) ->
        ((n, builds, discovers),
          fmap (\x -> (Result (buildDataId x) (buildDataProject x) (buildDataBuild x) (buildDataRef x) (fromMaybe BuildOk $ buildDataResult x))) .
          filter (\d ->
            Just (buildDataId d) ==
            (fmap buildDataId .
            head .
            reverse .
            (sortOn buildDataId) .
            filter ((==) (buildDataProject d) . buildDataProject) .
            filter ((==) (buildDataBuild d) . buildDataBuild) .
            filter ((==) (buildDataRef d) . buildDataRef) $
            builds)) .
          filter (isJust . buildDataResult) .
          filter ((==) (Just . Ref $ "refs/heads/master") . buildDataRef)
          $ builds)

complete :: Store -> BuildId -> BuildResult -> EitherT StoreError IO ()
complete store buildid result =
  case store of
    DynamoStore env e ->
      Mismi.runAWST env DynamoBackendError $ do
        r <- lift $ Dynamo.complete e buildid result
        firstT (ResultsError . Dynamo.jsonError) . for_ r $
          Dynamo.add e
    PostgresStore pool ->
      firstT (PostgresBackendError) . Traction.runDb pool $
        void $ Postgres.complete buildid result
    MemoryStore refx -> do
      now <- liftIO $ Time.getCurrentTime
      let
        update x =
          case buildDataId x == buildid of
            True ->
              x { buildDataEndTime = Just now, buildDataResult = Just result}
            False ->
              x
      liftIO $ IORef.atomicModifyIORef' refx $ \(n, builds, discovers) ->
        ((n, update <$> builds, discovers), ())

discover :: Store -> BuildId -> Project -> EitherT StoreError IO ()
discover store buildid project =
  case store of
    DynamoStore env _e ->
      Mismi.runAWST env DynamoBackendError $ do        -- FIX MTH maybe should store something here?
        pure ()
    PostgresStore pool ->
      firstT (PostgresBackendError) . Traction.runDb pool $
        void $ Postgres.discover buildid project
    MemoryStore _refx -> do
      pure ()

userByGithubId :: Store -> GithubId -> EitherT StoreError IO (Maybe User)
userByGithubId store userId =
  case store of
    DynamoStore _env _e ->
      left $ UnsupportedBackendError "authentication" "dynamo"
    PostgresStore pool ->
      firstT (PostgresBackendError) . Traction.runDb pool $
        Postgres.userByGithubId userId
    MemoryStore _refx -> do
      left $ UnsupportedBackendError "authentication" "memory"

updateUser :: Store -> User -> EitherT StoreError IO ()
updateUser store user =
  case store of
    DynamoStore _env _e ->
      left $ UnsupportedBackendError "authentication" "dynamo"
    PostgresStore pool ->
      firstT PostgresBackendError . Traction.runDb pool $
        Postgres.updateUser user
    MemoryStore _refx -> do
      left $ UnsupportedBackendError "authentication" "memory"

addUser :: Store -> GithubUser -> EitherT StoreError IO User
addUser store user =
  case store of
    DynamoStore _env _e ->
      left $ UnsupportedBackendError "authentication" "dynamo"
    PostgresStore pool ->
      firstT PostgresBackendError . Traction.runDb pool $
        Postgres.addUser user
    MemoryStore _refx -> do
      left $ UnsupportedBackendError "authentication" "memory"

newSession :: Store -> Session -> User -> EitherT StoreError IO ()
newSession store session user =
  case store of
    DynamoStore _env _e ->
      left $ UnsupportedBackendError "authentication" "dynamo"
    PostgresStore pool ->
      firstT PostgresBackendError . Traction.runDb pool $
        Postgres.newSession session user
    MemoryStore _refx -> do
      left $ UnsupportedBackendError "authentication" "memory"

tickSession :: Store -> SessionId -> EitherT StoreError IO ()
tickSession store session =
  case store of
    DynamoStore _env _e ->
      left $ UnsupportedBackendError "authentication" "dynamo"
    PostgresStore pool ->
      firstT PostgresBackendError . Traction.runDb pool $
        Postgres.tickSession session
    MemoryStore _refx -> do
      left $ UnsupportedBackendError "authentication" "memory"

getSessionUser :: Store -> SessionId -> EitherT StoreError IO (Maybe UserId)
getSessionUser store session =
  case store of
    DynamoStore _env _e ->
      left $ UnsupportedBackendError "authentication" "dynamo"
    PostgresStore pool ->
      firstT PostgresBackendError . Traction.runDb pool $
        Postgres.getSessionUser session
    MemoryStore _refx -> do
      left $ UnsupportedBackendError "authentication" "memory"

getSessionOAuth :: Store -> SessionId -> EitherT StoreError IO (Maybe GithubOAuth)
getSessionOAuth store session =
  case store of
    DynamoStore _env _e ->
      left $ UnsupportedBackendError "authentication" "dynamo"
    PostgresStore pool ->
      firstT PostgresBackendError . Traction.runDb pool $
        Postgres.getSessionOAuth session
    MemoryStore _refx -> do
      left $ UnsupportedBackendError "authentication" "memory"
