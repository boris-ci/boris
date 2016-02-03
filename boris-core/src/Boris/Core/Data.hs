{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Boris.Core.Data (
    Environment (..)
  , Project (..)
  , Build (..)
  , BuildId (..)
  , Repository (..)
  , LocalRepository (..)
  , Ref (..)
  , Query (..)
  , Executor (..)
  , Command (..)
  , BuildQuery (..)
  , Specification (..)
  , Registration (..)
  , BuildResult (..)
  , Acknowledge (..)
  , renderRegistration
  , parseRegistration
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Map.Strict as M

import           P

newtype Environment =
  Environment {
      renderEnvironment :: Text
    } deriving (Eq, Show)

newtype Project =
  Project {
      renderProject :: Text
    } deriving (Eq, Show, Ord)

newtype Build =
  Build {
      renderBuild :: Text
    } deriving (Eq, Show, Ord)

newtype BuildId =
  BuildId {
      renderBuildId :: Text
    } deriving (Eq, Show, Ord)

newtype Repository =
  Repository {
      renderRepository :: Text
    } deriving (Eq, Show, Ord)

newtype LocalRepository =
  LocalRepository {
      renderLocalRepository :: Text
    } deriving (Eq, Show, Ord)

newtype Ref =
  Ref {
      renderRef :: Text
    } deriving (Eq, Show, Ord)

newtype Query =
  Query {
      renderQuery :: Text
    } deriving (Eq, Show, Ord)

newtype Executor =
  Executor {
      projects :: M.Map Project Repository
    } deriving (Eq, Show, Ord)

data Command =
  Command {
      commandName :: Text
    , commandArgs :: [Text]
    } deriving (Eq, Show)

data BuildQuery =
  BuildQuery {
      buildName :: Build
    , buildQuery :: Query
    } deriving (Eq, Show)

data Specification =
  Specification {
      specificationBuild :: Build
    , specificationPre :: [Command]
    , specificationCommand :: [Command]
    , specificationPost :: [Command]
    , specificationSuccess :: [Command]
    , specificationFailure :: [Command]
    } deriving (Eq, Show)

data Registration =
  Registration {
      registrationProject :: Project
    , registrationRepository :: Repository
    } deriving (Eq, Show)

data BuildResult =
    BuildOk
  | BuildKo
    deriving (Eq, Show)

data Acknowledge =
    Accept
  | AlreadyRunning
    deriving (Eq, Show)

renderRegistration :: Registration -> Text
renderRegistration r =
  T.intercalate "=" [renderProject . registrationProject $ r, renderRepository . registrationRepository $ r]

parseRegistration :: Text -> Maybe Registration
parseRegistration t =
  case T.splitOn "=" t of
    [p,r] ->
      Just $ Registration (Project p) (Repository r)
    _ ->
      Nothing
