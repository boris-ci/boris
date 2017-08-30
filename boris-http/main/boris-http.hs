{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           Airship (defaultAirshipConfig, resourceToWai)

import           Boris.Core.Data
import           Boris.Http.Airship
import           Boris.Http.Config
import qualified Boris.Http.Resource.Static as Static
import           Boris.Http.Route (boris)
import qualified Boris.Store.Lifecycle as SL
import           Boris.Queue (BuildQueue (..))

import           Mismi (runAWST, discoverAWSEnv, renderRegionError, renderError)
import           Mismi.DynamoDB.Control (configureRetries)

import           Network.Wai.Handler.Warp (runEnv)

import           P

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
  orDie id $ runAWST cenv renderError $ SL.initialise e

  runEnv 10080 . Static.staticMiddleware $
    resourceToWai defaultAirshipConfig (boris l cenv e q c) resource404
