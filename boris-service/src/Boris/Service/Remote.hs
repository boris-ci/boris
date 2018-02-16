{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Remote (
    RemoteError (..)
  , renderRemoteError
  , heartbeat
  , acknowledge
  , avow
  , complete
  ) where

import qualified Boris.Client.Build as Build
import qualified Boris.Client.Http as Http
import           Boris.Core.Data
import           Boris.Service.Boot

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Morph (hoist)

import           P

import           X.Control.Monad.Trans.Either (EitherT)


data RemoteError =
    RemoteHttpError Http.BorisHttpClientError
    deriving (Show)

renderRemoteError :: RemoteError -> Text
renderRemoteError err =
  case err of
    RemoteHttpError e ->
      mconcat ["Remote API Error: ", Http.renderBorisHttpClientError e]

heartbeat :: MonadIO m => BuildService -> BuildId -> EitherT RemoteError m BuildCancelled
heartbeat service buildId =
  case service of
    PushBuild http ->
      hoist liftIO . firstT RemoteHttpError $
        Build.heartbeat http buildId
    LogBuild ->
      pure BuildNotCancelled


acknowledge :: MonadIO m => BuildService -> BuildId -> EitherT RemoteError m Acknowledge
acknowledge service buildId =
  case service of
    PushBuild http ->
      hoist liftIO . firstT RemoteHttpError $
        Build.acknowledge http buildId
    LogBuild ->
      pure Accept

avow :: MonadIO m => BuildService -> BuildId -> Ref -> Commit -> EitherT RemoteError m ()
avow service buildId ref commit =
  case service of
    PushBuild http ->
      hoist liftIO . firstT RemoteHttpError $
        Build.avow http buildId ref commit
    LogBuild ->
      pure ()

complete :: MonadIO m => BuildService -> BuildId -> BuildResult -> EitherT RemoteError m ()
complete service buildId result =
  case service of
    PushBuild http ->
      hoist liftIO . firstT RemoteHttpError $
        Build.complete http buildId result
    LogBuild ->
      pure ()
