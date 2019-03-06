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

import           Boris.Client.Error
import qualified Boris.Client.Build as Build
import qualified Boris.Client.Network as Network
import           Boris.Core.Data.Build
import           Boris.Service.Boot
import           Boris.Prelude

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Morph (hoist)


data RemoteError =
    RemoteHttpError BorisError
    deriving (Show)

renderRemoteError :: RemoteError -> Text
renderRemoteError err =
  case err of
    RemoteHttpError e ->
      mconcat ["Remote API Error: ", renderBorisError e]

heartbeat :: MonadIO m => BuildService -> BuildId -> EitherT RemoteError m BuildCancelled
heartbeat service buildId =
  error "Todo"
  {--
  case service of
    PushBuild http ->
      hoist liftIO . firstT RemoteHttpError $
        Build.heartbeat http buildId
    LogBuild ->
      pure BuildNotCancelled
--}

acknowledge :: MonadIO m => BuildService -> BuildId -> EitherT RemoteError m Acknowledge
acknowledge service buildId =
  error "todo"
  {--
  case service of
    PushBuild http ->
      hoist liftIO . firstT RemoteHttpError $
        Build.acknowledge http buildId
    LogBuild ->
      pure Accept
--}
avow :: MonadIO m => BuildService -> BuildId -> Ref -> Commit -> EitherT RemoteError m ()
avow service buildId ref commit =
  error "todo"
  {--
  case service of
    PushBuild http ->
      hoist liftIO . firstT RemoteHttpError $
        Build.avow http buildId ref commit
    LogBuild ->
      pure ()
--}

complete :: MonadIO m => BuildService -> BuildId -> BuildResult -> EitherT RemoteError m ()
complete service buildId result =
  error "todo"
  {--
  case service of
    PushBuild http ->
      hoist liftIO . firstT RemoteHttpError $
        Build.complete http buildId result
    LogBuild ->
      pure ()
--}
