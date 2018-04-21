{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Boot (
    Boot (..)
  , boot

  , Mode (..)
  , AuthenticationMode (..)
  , BuildService (..)
  ) where

import           Boris.Core.Data
import           Boris.Http.Data
import           Boris.Queue (BuildQueue (..), Request (..), RequestBuild (..), RequestDiscover (..))
import qualified Boris.Service.Boot as Service
import qualified Boris.Service.Build as Build
import qualified Boris.Service.Discover as Discover

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Chan as Chan
import           Control.Monad.IO.Class (MonadIO (..))

import           Data.ByteString (ByteString)
import qualified Data.Map as Map
import qualified Data.Text.IO as Text

import           Mismi.Environment (Env)
import           Mismi.S3.Core.Data (Address (..), addressFromText)

import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           Nest (Parser)
import qualified Nest

import           P

import           Snooze.Balance.Control (BalanceConfig (..))
import           Snooze.Balance.Data (BalanceEntry (..), BalanceTable (..), Host (..), Port (..), balanceTableStatic)

import           System.IO (IO)

import           Traction.Control (DbPool)
import qualified Traction.Control as Traction

import           X.Control.Monad.Trans.Either (runEitherT)


data Mode =
    DevelopmentMode
  | ProductionMode
  | TestMode
    deriving (Eq, Show)

data AuthenticationMode =
    GithubAuthentication Manager GithubClient GithubSecret
  | NoAuthentication

data BuildService =
    SqsBuildService Env BuildQueue
  | EcsBuildService
  | LocalBuildService (Chan.Chan Request)

data Boot =
  Boot Mode AuthenticationMode BuildService DbPool (Maybe Settings)

boot :: MonadIO m => IO Env -> Parser m Boot
boot mkEnv = do
  mode <- Nest.setting "BORIS_MODE" (Map.fromList [
      ("production", ProductionMode)
    , ("development", DevelopmentMode)
    ]) `Nest.withDefault` ProductionMode

  pool <- postgres

  auth <- join $ Nest.setting "BORIS_AUTHENTICATION" (Map.fromList [
      ("github", github)
    , ("none", pure NoAuthentication)
    ]) `Nest.withDefault` github

  worker <- join $ Nest.setting "BORIS_BUILD_SERVICE" (Map.fromList [
      ("sqs", sqs mkEnv)
    , ("ecs", ecs)
    , ("local", local)
    ]) `Nest.withDefault` sqs mkEnv

  settings <- Nest.option $ Nest.setting "BORIS_TENANCY" (Map.fromList [
      ("single", SingleTenantSettings)
    , ("multi", MultiTenantSettings)
    ])

  pure $ Boot mode auth worker pool settings

github :: MonadIO m => Parser m AuthenticationMode
github = do
  manager <- liftIO $ newManager tlsManagerSettings
  client <- GithubClient <$> Nest.string "BORIS_GITHUB_CLIENT"
  secret <- GithubSecret <$> Nest.string "BORIS_GITHUB_SECRET"
  pure $ GithubAuthentication manager client secret

sqs :: MonadIO m => IO Env -> Parser m BuildService
sqs mkEnv =
  SqsBuildService
    <$> liftIO mkEnv
    <*> (BuildQueue <$> Nest.string "BORIS_BUILD_QUEUE")

ecs :: Monad m => Parser m BuildService
ecs =
  pure EcsBuildService

local :: MonadIO m => Parser m BuildService
local = do
  port <- Port <$> Nest.numeric "PORT" `Nest.withDefault` 9999
  mgr <- liftIO $ newManager tlsManagerSettings
  t <- balanceTableStatic $ BalanceTable [BalanceEntry (Host "localhost") port]
  let http = BalanceConfig t mempty mgr
  channel <- liftIO $ Chan.newChan
  liftIO . void . Async.async $
    let
      go = do
        request <- Chan.readChan channel
        case request of
          RequestBuild' (RequestBuild buildId project repository build ref) -> do
            result <- runEitherT $
              Build.builder Service.Std (Service.PushBuild http) (WorkspacePath "tmp") buildId project repository build ref
            case result of
              Left err ->
                Text.putStrLn . Build.renderBuilderError $ err
              Right _ ->
                pure ()
          RequestDiscover' (RequestDiscover buildId project repository) -> do
            result <- runEitherT $
              Discover.discover Service.Std (Service.PushDiscover http) (WorkspacePath "tmp") buildId project repository
            case result of
              Left err ->
                Text.putStrLn . Discover.renderDiscoverError $ err
              Right _ ->
                pure ()
        go
    in
      go
  pure $ LocalBuildService channel

postgres :: MonadIO m => Parser m DbPool
postgres = do
  conn <- Nest.string "BORIS_POSTGRES"
  liftIO $ Traction.newPool conn

address :: Monad m => ByteString -> Parser m Address
address name = do
  s <- Nest.string name
  case addressFromText s of
    Nothing ->
        Nest.failure name $ mconcat [
            "Could not parse s3 address [", s, "]"
          ]
    Just a ->
      pure a
