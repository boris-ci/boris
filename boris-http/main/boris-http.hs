{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           Airship (defaultAirshipConfig, resourceToWai)

import           Agriculture (agriculture)

import           Boris.Core.Data
import           Boris.Http.Config
import           Boris.Http.Route (boris)
import qualified Boris.Store.Lifecycle as SL
import           Boris.Queue (BuildQueue (..))

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
  (q, e, c, l) <- orDie renderHttpConfigError $ (,,,)
    <$> (BuildQueue <$> text "BORIS_BUILD_QUEUE")
    <*> (Environment <$> text "BORIS_ENVIRONMENT")
    <*> configLocation
    <*> clientLocale
  env <- orDie renderRegionError discoverAWSEnv
  orDie renderError $ runAWS env $ SL.initialise e

  runStopFile (lookupEnv "BORIS_HTTP_STOP") $ \pin -> do
    agriculture pin "boris-http" buildInfoVersion $ do
      return $ resourceToWai defaultAirshipConfig (boris l env e q c) (resource404 ())
