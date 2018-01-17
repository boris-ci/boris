{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Repository (
    ConfigError (..)
  , pick
  , list
  , renderConfigError
  ) where

import           Boris.Core.Data
import           Boris.Http.Data

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (runResourceT)

import           Data.Conduit (($=+), ($$+-))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT

import           Mismi (Error, renderError, runAWST)
import           Mismi.Amazonka (Env)
import qualified Mismi.S3 as S3

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, mapEitherT, left)

data ConfigError =
    ConfigFileNotFoundError ConfigurationMode
  | ConfigAwsError Error
  | ConfigParseError Text

pick :: Env -> ConfigurationMode -> Project -> EitherT ConfigError IO (Maybe Repository)
pick env mode project =
  case mode of
    GlobalS3WhitelistMode location -> do
      registration <- runAWST env ConfigAwsError $ do
        attempt <- lift $ S3.read' location
        source <- fromMaybeM (left $ ConfigFileNotFoundError mode) $
          attempt
        mapEitherT (liftIO . runResourceT) $ (hoist lift $ source)
          $=+ CT.decodeUtf8
          $=+ CT.lines
          $=+ CL.mapM (\t -> fromMaybeM (left $ ConfigParseError t) $ parseRegistration t)
          $=+ CL.filter ((==) project . registrationProject)
          $$+- CL.head

      pure $ registrationRepository <$> registration


list :: Env -> ConfigurationMode -> EitherT ConfigError IO [Project]
list env mode =
  case mode of
    GlobalS3WhitelistMode location ->
      runAWST env ConfigAwsError $ do
        attempt <- lift $ S3.read' location
        source <- fromMaybeM (left $ ConfigFileNotFoundError mode) $
          attempt
        mapEitherT (liftIO . runResourceT) $ (hoist lift $ source)
          $=+ CT.decodeUtf8
          $=+ CT.lines
          $=+ CL.mapM (\t -> fromMaybeM (left $ ConfigParseError t) $ parseRegistration t)
          $=+ CL.map registrationProject
          $$+- CL.consume

renderConfigError :: ConfigError -> Text
renderConfigError err =
  case err of
    ConfigFileNotFoundError (GlobalS3WhitelistMode l) ->
      mconcat ["Service is mis-configured, repository configuration could not be found: ", S3.addressToText l]
    ConfigAwsError e ->
      mconcat ["Service could not retrieve repository configuration: ", renderError e]
    ConfigParseError t ->
      mconcat ["Service could not parse repostository configuraiton: ", t]
