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

import           Data.String (String)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Zones (TZ, loadLocalTZ)
import           Data.Time.Zones.All (tzByName)

import           Control.Monad.IO.Class (liftIO)

import           Mismi.S3 (Address, addressFromText)

import           P

import           System.IO (IO)

import qualified System.Environment as E

import           X.Control.Monad.Trans.Either (EitherT, eitherTFromMaybe, left)


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
text v =
  MissingEnvironmentVariableHttpConfigError v `eitherTFromMaybe` lookupEnv v

addr :: Text -> EitherT HttpConfigError IO Address
addr v = do
  a <- text v
  (left . InvalidAddressHttpConfigError) a `fromMaybeM` addressFromText a

timeZoneOrLocal :: Text -> EitherT HttpConfigError IO TZ
timeZoneOrLocal =
  fromMaybeM (liftIO loadLocalTZ) <=< timeZone

timeZone :: Text -> EitherT HttpConfigError IO (Maybe TZ)
timeZone v = do
  mt <- liftIO $ lookupEnv v
  for mt $ \t ->
    fromMaybeM (left $ UnknownLocationHttpConfigError t) . tzByName . T.encodeUtf8 $ t


lookupEnv :: Text -> IO (Maybe Text)
lookupEnv =
  (fmap . fmap) T.pack . E.lookupEnv . T.unpack


newtype WrappedFail a =
  WrappedFail {
      runWrappedFail :: Either String a
    } deriving (Eq, Show, Functor, Applicative)

instance Monad WrappedFail where
  m >>= f = WrappedFail $ runWrappedFail m >>= runWrappedFail . f
  fail = WrappedFail . Left
