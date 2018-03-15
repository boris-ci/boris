{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Boot (
    Boot (..)
  , boot

  , LogService (..)
  , DiscoverService (..)
  , BuildService (..)
  ) where

import           Control.Monad.IO.Class (MonadIO (..))

import qualified Data.Map as Map

import           Nest (Parser)
import qualified Nest

import           P

import           Snooze.Balance.Control (BalanceConfig (..))

import           System.IO (IO)

data LogService =
    Std

data DiscoverService =
    PushDiscover BalanceConfig
  | LogDiscover

data BuildService =
    PushBuild BalanceConfig
  | LogBuild

data Boot =
  Boot LogService DiscoverService BuildService

boot :: MonadIO m => IO BalanceConfig -> Parser m Boot
boot mkHttp = do
  logs <- join $ Nest.setting "BORIS_LOG_SERVICE" (Map.fromList [
      ("std", std)
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

std :: Monad m => Parser m LogService
std =
  pure Std
