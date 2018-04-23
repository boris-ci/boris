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

import           Control.Monad.IO.Class (MonadIO (..))

import qualified Data.Map as Map

import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           Nest (Parser)
import qualified Nest

import           P

import           Traction.Control (DbPool)
import qualified Traction.Control as Traction


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

boot :: MonadIO m => Parser m Boot
boot = do
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
