{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Boris.Http.Boot as Boot
import qualified Boris.Http.Route as Route
import qualified Boris.Http.Store.Api as Store
import qualified Boris.Http.Store.Error as Store

import qualified Nest

import           Mismi (discoverAWSEnv, renderRegionError)
import           Mismi.DynamoDB.Control (configureRetries)

import           P

import qualified System.IO as IO
import           System.IO (IO)

import qualified Web.Spock.Core as Spock

import           X.Control.Monad.Trans.Either.Exit (orDie)


import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setOnException, defaultOnException)

main :: IO ()
main = do
  let
    mkEnv = do
     env <- orDie renderRegionError discoverAWSEnv
     pure $ configureRetries env

  Boot.Boot mode _authentication builds logs projectx store <-
    Nest.force $ Boot.boot mkEnv

  orDie Store.renderStoreError $
    Store.initialise store

  port <- Nest.force $ Nest.numeric "PORT" `Nest.withDefault` 10080

  app <- Spock.spockAsApp $ Spock.spockConfigT Spock.defaultSpockConfig id (Route.route store builds logs projectx mode)

  let
    s = setPort port $
      setOnException (\req ex -> do
        IO.print ex
        defaultOnException req ex) $
          defaultSettings
  runSettings s app
