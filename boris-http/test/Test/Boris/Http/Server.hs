{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Boris.Http.Server (
    withServer
  , withServerX
  , withServerT
  ) where

import           Control.Exception (bracket)

import qualified Control.Concurrent.Async as Async
import           Control.Monad.IO.Class (MonadIO (..))

import           Boris.Core.Data

import qualified Boris.Http.Boot as Boot
import qualified Boris.Http.Route as Route
import           Boris.Http.Store.Data

import qualified Data.IORef as IORef

import           Hedgehog

import qualified Data.Streaming.Network as Network

import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.Socket as Socket
import qualified Network.Wai.Handler.Warp as Warp

import           P
import qualified Prelude as Unsafe

import           Snooze.Balance.Control (BalanceConfig (..))
import           Snooze.Balance.Data (BalanceEntry (..), BalanceTable (..), Host (..), Port (..), balanceTableStatic)

import           System.IO (IO)

import qualified Web.Spock.Core as Spock
import           X.Control.Monad.Trans.Either (EitherT, newEitherT, runEitherT)

withServerX :: (MonadIO m, MonadTest m) => (BalanceConfig -> IO a) -> m a
withServerX =
  evalIO . withServer

withServerT :: (MonadIO m, MonadTest m, Show e) => (BalanceConfig -> EitherT e IO a) -> m a
withServerT testing =
  evalExceptT . newEitherT $ withServerX (runEitherT . testing)

withServer :: (BalanceConfig -> IO a) -> IO a
withServer testing = do
  ref <- IORef.newIORef (1, [], [])
  app <- Spock.spockAsApp $ Spock.spockConfigT Spock.defaultSpockConfig id (Route.route (MemoryStore ref) Boot.NoAuthentication Boot.EcsBuildService Boot.DevNull (Boot.SingleProjectMode (Project "demo") (Repository "master")) Boot.TestMode)
  mgr <- newManager tlsManagerSettings
  Socket.withSocketsDo $
    bracket
      (Network.bindPortTCP 0 "127.0.0.1")
      Socket.close
      (\socket -> do
        name <- Socket.getSocketName socket
        case name of
          Socket.SockAddrInet port _ -> do
            Async.withAsync (Warp.runSettingsSocket Warp.defaultSettings socket app) $ \_ -> do
              t <- balanceTableStatic $ BalanceTable [BalanceEntry (Host "127.0.0.1") (Port $ fromIntegral port)]
              testing (BalanceConfig t mempty mgr)
          _ -> do
            Unsafe.error "<invarient> forcing inet above.")
