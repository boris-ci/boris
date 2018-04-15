{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Boris.Http.Boot as Boot
import qualified Boris.Http.Route as Route
import qualified Boris.Http.Db.Schema as Schema

import qualified Nest

import           Mismi (discoverAWSEnv, renderRegionError)

import           P

import qualified System.IO as IO
import           System.IO (IO)

import qualified Traction.Control as Traction

import qualified Web.Spock.Core as Spock

import           X.Control.Monad.Trans.Either.Exit (orDie)


import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setOnException, defaultOnException)

main :: IO ()
main = do
  let
    mkEnv = do
     orDie renderRegionError discoverAWSEnv

  Boot.Boot mode authentication builds logs projectx pool <-
    Nest.force $ Boot.boot mkEnv

  orDie Traction.renderDbError $
    Schema.initialise pool

  port <- Nest.force $ Nest.numeric "PORT" `Nest.withDefault` 10080

  app <- Spock.spockAsApp $ Spock.spockConfigT Spock.defaultSpockConfig id (Route.route pool authentication builds logs projectx mode)

  let
    s = setPort port $
      setOnException (\req ex -> do
        IO.print ex
        defaultOnException req ex) $
          defaultSettings
  runSettings s app
