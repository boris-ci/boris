{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           Airship (defaultAirshipConfig, resourceToWai)

import           Agriculture (agriculture)

import           Boris.Core.Data
import           Boris.Http.Config
import qualified Boris.Http.Resource.Static as Static
import           Boris.Http.Route (boris)
import qualified Boris.Store.Lifecycle as SL
import           Boris.Queue (BuildQueue (..))

import           BuildInfo_ambiata_boris_http (buildInfoVersion)


import           Charlotte.Airship (resource404)
import           Clerk.QuickStop (runStopFile)

import           Mismi (runAWS, discoverAWSEnv, renderRegionError, renderError)
import           Mismi.DynamoDB.Control (configureRetries)

import           P

import           System.Environment (lookupEnv)
import           System.IO (IO)

import           X.Control.Monad.Trans.Either.Exit (orDie)


main :: IO ()
main = do
  (q, e, c, l) <- orDie renderHttpConfigError $ (,,,)
    <$> (BuildQueue <$> text "BORIS_BUILD_QUEUE")
    <*> (Environment <$> text "BORIS_ENVIRONMENT")
    <*> configLocation "BORIS_CONFIG_LOCATION"
    <*> clientLocale "BORIS_CLIENT_TIMEZONE"
  env <- orDie renderRegionError discoverAWSEnv
  let
    cenv = configureRetries env
  orDie renderError $ runAWS cenv $ SL.initialise e

  runStopFile (lookupEnv "BORIS_HTTP_STOP") $ \pin -> do
    agriculture pin "boris-http" buildInfoVersion $ do
      return . ($) Static.staticMiddleware $
        resourceToWai defaultAirshipConfig (boris l cenv e q c) (resource404 ())
