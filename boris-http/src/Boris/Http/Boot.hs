{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Boot (
    Boot (..)
  , boot

  , Mode (..)
  , AuthenticationMode (..)
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

data Boot =
  Boot Mode AuthenticationMode DbPool (Maybe Settings)

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

  settings <- Nest.option $ Nest.setting "BORIS_TENANCY" (Map.fromList [
      ("single", SingleTenantSettings)
    , ("multi", MultiTenantSettings)
    ])

  pure $ Boot mode auth pool settings

github :: MonadIO m => Parser m AuthenticationMode
github = do
  manager <- liftIO $ newManager tlsManagerSettings
  client <- GithubClient <$> Nest.string "BORIS_GITHUB_CLIENT"
  secret <- GithubSecret <$> Nest.string "BORIS_GITHUB_SECRET"
  pure $ GithubAuthentication manager client secret

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
