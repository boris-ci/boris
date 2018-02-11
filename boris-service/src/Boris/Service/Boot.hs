{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Boot (
    Boot (..)
  , boot

  , LogService (..)
  , DiscoverService (..)
  , BuildService (..)
  ) where

import           Boris.Core.Data

import           Control.Monad.IO.Class (MonadIO (..))

import qualified Data.Map as Map

import           Mismi.Environment (Env)

import qualified Nest
import           Nest (Parser)

import           P

import           Snooze.Balance.Control (BalanceConfig (..))

import           System.IO (IO)

data LogService =
    CloudWatchLogs Env Environment
  | Std

data DiscoverService =
    PushDiscover BalanceConfig
  | LogDiscover

data BuildService =
    PushBuild BalanceConfig
  | LogBuild

data Boot =
  Boot LogService DiscoverService BuildService

boot :: MonadIO m => IO Env -> IO BalanceConfig -> Parser m Boot
boot mkEnv mkHttp = do
  logs <- join $ Nest.setting "BORIS_LOG_SERVICE" (Map.fromList [
      ("cloudwatch", cloudwatch mkEnv)
    , ("std", std)
    ]) `Nest.withDefault` std

  discover <- join $ Nest.setting "BORIS_DISCOVER_SERVICE_NOTIFICATION" (Map.fromList [
      ("http", PushDiscover <$> liftIO mkHttp)
    , ("log", pure LogDiscover)
    ]) `Nest.withDefault` pure LogDiscover

  build <- join $ Nest.setting "BORIS_BUILD_SERVICE_NOTIFICATION" (Map.fromList [
      ("http", PushBuild <$> liftIO mkHttp)
    , ("log", pure LogBuild)
    ]) `Nest.withDefault` pure LogBuild

  pure $ Boot logs discover build

cloudwatch :: MonadIO m => IO Env -> Parser m LogService
cloudwatch mkEnv =
  CloudWatchLogs
    <$> liftIO mkEnv
    <*> (Environment <$> Nest.string "BORIS_ENVIRONMENT")

std :: Monad m => Parser m LogService
std =
  pure Std
