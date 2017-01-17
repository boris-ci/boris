{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           Airship (defaultAirshipConfig, resourceToWai)

import           Agriculture (agriculture)

import           Boris.Core.Data
import           Boris.Http.Config
import qualified Boris.Http.Resource.Static as Static
import           Boris.Http.Data
import           Boris.Http.Route (borisReadonly)
import qualified Boris.Store.Lifecycle as SL

import           BuildInfo_ambiata_boris_http (buildInfoVersion)

import           Charlotte.Airship (resource404)
import           Clerk.QuickStop (runStopFile)

import           Mismi (runAWS, discoverAWSEnv, renderRegionError, renderError)

import           P

import           System.Environment (lookupEnv)
import           System.IO (IO)

import           X.Control.Monad.Trans.Either.Exit (orDie)


main :: IO ()
main = do
  (e, c) <- orDie renderHttpConfigError $ (,)
    <$> (Environment <$> text "BORIS_ENVIRONMENT")
    <*> (ConfigLocation <$> addr "BORIS_CONFIG_LOCATION")
  env <- orDie renderRegionError discoverAWSEnv
  orDie renderError $ runAWS env $ SL.initialise e

  runStopFile (lookupEnv "BORIS_HTTP_STOP") $ \pin -> do
    agriculture pin "boris-http-readonly" buildInfoVersion $
      return . ($) Static.staticMiddleware $
        resourceToWai defaultAirshipConfig (borisReadonly env e c) (resource404 ())
