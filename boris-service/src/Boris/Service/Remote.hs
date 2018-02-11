{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Remote (
    RemoteError (..)
  , renderRemoteError
  , heartbeat
  , acknowledge
  , disavow
  , avow
  , complete
  ) where

import           Boris.Core.Data
import qualified Boris.Client.Http as Http
import qualified Boris.Client.Build as Build
import           Boris.Service.Boot

import           Control.Monad.Morph (hoist)
import           Control.Monad.IO.Class (MonadIO (..))

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

-- FIX is this really needed or could it be inferred on server???
disavow :: MonadIO m => BuildService -> BuildId -> Project -> Build -> EitherT RemoteError m ()
disavow service buildId project build =
  case service of
    PushBuild http ->
      hoist liftIO . firstT RemoteHttpError $
        Build.disavow http buildId project build
    LogBuild ->
      pure ()

avow :: MonadIO m => BuildService -> BuildId -> Project -> Build -> Ref -> Commit -> EitherT RemoteError m ()
avow service buildId project build ref commit =
  case service of
    PushBuild http ->
      hoist liftIO . firstT RemoteHttpError $
        Build.avow http buildId project build ref commit
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
