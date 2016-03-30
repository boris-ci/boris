{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           Airship (defaultAirshipConfig, resourceToWai)

import           Agriculture (agriculture)

import           Boris.Core.Data
import           Boris.Http.Data
import           Boris.Http.Route (borisReadonly)
import qualified Boris.Store.Lifecycle as SL

import           BuildInfo_ambiata_boris_http (buildInfoVersion)

import           Charlotte.Airship (resource404)

import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Mismi (runAWS, discoverAWSEnv, renderRegionError, renderError)
import           Mismi.S3 (Address, addressFromText)

import           P

import           System.Environment (lookupEnv)
import           System.Exit (exitFailure)
import           System.IO (IO, stderr)

import           X.Control.Monad.Trans.Either.Exit (orDie)


main :: IO ()
main = do
  e <- Environment <$> text "BORIS_ENVIRONMENT"
  c <- ConfigLocation <$> addr "BORIS_CONFIG_LOCATION"
  env <- orDie renderRegionError discoverAWSEnv
  orDie renderError $ runAWS env $ SL.initialise e
  agriculture "boris-http-readonly" buildInfoVersion $ do
    pure $ resourceToWai defaultAirshipConfig (borisReadonly env e c) (resource404 ())

text :: String -> IO Text
text e =
  lookupEnv e >>=
    maybe (bomb . T.pack $ e <> " is a required environment variable to start boris-http.") (pure . T.pack)

addr :: String -> IO Address
addr e =
  text e >>=
    fromMaybeM (bomb . T.pack $ e <> " is not a valid s3 address.") . addressFromText

bomb :: Text -> IO a
bomb msg =
  T.hPutStrLn stderr msg >> exitFailure
