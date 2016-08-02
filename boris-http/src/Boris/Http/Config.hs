{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Config (
    HttpConfigError (..)
  , renderHttpConfigError
  , configLocation
  , clientLocale
  , addr
  , text
  ) where

import           Boris.Http.Data

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Zones (TZ, loadLocalTZ)
import           Data.Time.Zones.All (tzByName)

import           Control.Arrow ((&&&))
import           Control.Monad.IO.Class (liftIO)

import           Mismi.S3 (Address, addressFromText)

import           P

import qualified System.Environment as E
import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, eitherTFromMaybe)


configLocation :: Text -> EitherT HttpConfigError IO ConfigLocation
configLocation =
  fmap ConfigLocation . addr

clientLocale :: Text -> EitherT HttpConfigError IO ClientLocale
clientLocale tze =
  ClientLocale
    <$> timeZoneOrLocal tze


data HttpConfigError =
    MissingEnvironmentVariableHttpConfigError Text
  | InvalidAddressHttpConfigError Text
  | UnknownLocationHttpConfigError Text
  deriving (Eq, Show)

renderHttpConfigError :: HttpConfigError -> Text
renderHttpConfigError = \case
  MissingEnvironmentVariableHttpConfigError x ->
    x <> " is a required environment variable to start boris-http."
  InvalidAddressHttpConfigError x ->
    x <> " is not a valid s3 address."
  UnknownLocationHttpConfigError x -> 
    "Unknown location: " <> x


text :: Text -> EitherT HttpConfigError IO Text
text =
  uncurry eitherTFromMaybe . (MissingEnvironmentVariableHttpConfigError &&& lookupEnv)

addr :: Text -> EitherT HttpConfigError IO Address
addr =
  text >=> 
    uncurry eitherTFromMaybe . (InvalidAddressHttpConfigError &&& return . addressFromText)

timeZone :: Text -> EitherT HttpConfigError IO (Maybe TZ)
timeZone = do
  liftIO . lookupEnv >=>
    traverse (uncurry eitherTFromMaybe . (UnknownLocationHttpConfigError &&& return . tzByName . T.encodeUtf8))

timeZoneOrLocal :: Text -> EitherT HttpConfigError IO TZ
timeZoneOrLocal =
  timeZone >=>
    fromMaybeM (liftIO loadLocalTZ)


lookupEnv :: Text -> IO (Maybe Text)
lookupEnv =
  (fmap . fmap) T.pack . E.lookupEnv . T.unpack
