{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Project (
    ConfigError (..)
  , pick
  , list
  , discover
  , renderConfigError
  ) where

import           Boris.Core.Data
import           Boris.Http.Boot
import qualified Boris.Http.Service as Service
import qualified Boris.Http.Store.Api as Store
import           Boris.Http.Store.Data
import qualified Boris.Http.Store.Error as Store
import           Boris.Queue (Request (..), RequestDiscover (..))

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (runResourceT)

import           Data.Conduit (($$+-), ($=+))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT

import           Mismi (Error, renderError, runAWST)
import qualified Mismi.S3 as S3

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, left, mapEitherT)


data ConfigError =
    ConfigFileNotFoundError ProjectMode
  | ConfigAwsError Error
  | ConfigParseError Text

pick :: ProjectMode -> Project -> EitherT ConfigError IO (Maybe Repository)
pick mode project =
  case mode of
    WhitelistProjectMode env location -> do
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
    UserProjectMode ->
      -- FIX MTH add in db backed users.
      pure Nothing
    SingleProjectMode p r ->
      pure $ if project == p then Just r else Nothing

list :: ProjectMode -> EitherT ConfigError IO [Project]
list mode =
  case mode of
    WhitelistProjectMode env location ->
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
    UserProjectMode ->
      -- FIX MTH add in db back users
      pure []
    SingleProjectMode p _ ->
      pure [p]

renderConfigError :: ConfigError -> Text
renderConfigError err =
  case err of
    ConfigFileNotFoundError (WhitelistProjectMode _ l) ->
      mconcat ["Service is mis-configured, repository configuration could not be found: ", S3.addressToText l]
    ConfigFileNotFoundError _ ->
      mconcat ["Service is mis-configured, config file was not found (but it looks like an alternative project mode is active)."]
    ConfigAwsError e ->
      mconcat ["Service could not retrieve repository configuration: ", renderError e]
    ConfigParseError t ->
      mconcat ["Service could not parse repostository configuraiton: ", t]

-- FIX MTH error type
discover :: Store -> BuildService -> ProjectMode -> Project -> EitherT Text IO (Maybe BuildId)
discover store buildx projectx project = do
  r <- firstT renderConfigError (pick projectx project)
  case r of
    Nothing ->
      pure Nothing
    Just repository -> do
      i <- firstT Store.renderStoreError $
        Store.tick store
      firstT Store.renderStoreError $
        Store.discover store i project
      let req = RequestDiscover' $ RequestDiscover i project repository
      firstT Service.renderServiceError $
        Service.put buildx req
      pure (Just i)
