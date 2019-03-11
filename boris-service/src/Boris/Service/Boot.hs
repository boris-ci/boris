{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Boot (
    Boot (..)
  , boot

  , LogService (..)
  , DiscoverService (..)
  , BuildService (..)
  ) where

import qualified Boris.Client.Config as Config
import           Boris.Prelude

import           Control.Monad.IO.Class (MonadIO (..))

import qualified Data.Map as Map

import           Nest (Parser)
import qualified Nest

import           System.IO (IO)


data LogService =
    Std
  | PushLog Config.Boris

data DiscoverService =
    PushDiscover Config.Boris
  | LogDiscover

data BuildService =
    PushBuild Config.Boris
  | LogBuild

data Boot =
  Boot LogService DiscoverService BuildService

boot :: MonadIO m => IO Config.Boris -> Parser m Boot
boot mkHttp = do
  logs <- join $ Nest.setting "BORIS_LOG_SERVICE" (Map.fromList [
      ("std", std)
    , ("http", PushLog <$> liftIO mkHttp)
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
