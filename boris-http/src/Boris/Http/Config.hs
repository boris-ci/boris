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
import           Data.Time (TimeZone, defaultTimeLocale, getCurrentTimeZone, parseTimeM)

import           Control.Monad.IO.Class (liftIO)

import           Mismi.S3 (Address, addressFromText)

import           P

import           System.IO (IO)

import qualified System.Environment as E

import           X.Control.Monad.Trans.Either (EitherT, eitherTFromMaybe, firstEitherT, left, newEitherT)


configLocation :: EitherT HttpConfigError IO ConfigLocation
configLocation =
  ConfigLocation <$> addr "BORIS_CONFIG_LOCATION"

clientLocale :: EitherT HttpConfigError IO ClientLocale
clientLocale =
  ClientLocale
    <$> timeZoneOrLocal "BORIS_CLIENT_TIMEZONE"


data HttpConfigError =
    MissingEnvironmentVariableHttpConfigError Text
  | InvalidAddressHttpConfigError Text
  | InvalidTimeZoneHttpConfigError Text Text
  deriving (Eq, Show)

renderHttpConfigError :: HttpConfigError -> Text
renderHttpConfigError = \case
  MissingEnvironmentVariableHttpConfigError x ->
    x <> " is a required environment variable to start boris-http."
  InvalidAddressHttpConfigError x ->
    x <> " is not a valid s3 address."
  InvalidTimeZoneHttpConfigError x e -> 
    "Error parsing TimeZone value '" <> x <> "'. Parser error: " <> e


text :: Text -> EitherT HttpConfigError IO Text
text v =
  MissingEnvironmentVariableHttpConfigError v `eitherTFromMaybe` lookupEnv v

addr :: Text -> EitherT HttpConfigError IO Address
addr v = do
  a <- text v
  (left . InvalidAddressHttpConfigError) a `fromMaybeM` addressFromText a

timeZoneOrLocal :: Text -> EitherT HttpConfigError IO TimeZone
timeZoneOrLocal =
  fromMaybeM (liftIO getCurrentTimeZone) <=< timeZone

timeZone :: Text -> EitherT HttpConfigError IO (Maybe TimeZone)
timeZone v =
  let parseTimeZone = runWrappedFail . parseTimeM True defaultTimeLocale "%z" . T.unpack
  in firstEitherT (InvalidTimeZoneHttpConfigError v . T.pack) . newEitherT $ traverse parseTimeZone <$> lookupEnv v

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
