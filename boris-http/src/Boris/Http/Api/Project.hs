{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Project (
    picker
  , pick
  , list
  , discover

  , NewProjectError (..)
  , new
  ) where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Project
import           Boris.Core.Data.Repository
import           Boris.Core.Data.Tenant
import           Boris.Http.Data
import qualified Boris.Http.Db.Project as ProjectDb
import           Boris.Prelude

import qualified Data.List as List

import           System.IO (IO)

import           Traction.Sql (Unique (..))
import           Traction.Control (Db, DbPool, DbError)
import qualified Traction.Control as Traction


picker :: ProjectName -> [Project] -> Maybe Project
picker project =
  List.find ((==) project . projectName)


pick :: DbPool -> Tenant -> AuthenticatedBy -> Project -> EitherT DbError IO (Maybe Project)
pick pool tenant authenticated project =
  error "Todo"
  {--
  Traction.runDb pool $ case tenant of
    SingleTenant ->
      case authenticated of
        AuthenticatedByDesign _ ->
          picker project <$> Query.getAllProjects
        AuthenticatedByGithub _session user ->
           picker project <$> Query.getAccountProjects (userIdOf user)
    MultiTenant ->
      case authenticated of
        AuthenticatedByDesign _ ->
          pure Nothing
        AuthenticatedByGithub _session user ->
          picker project <$> Query.getAccountProjects (userIdOf user)
--}
list :: DbPool -> Tenant -> AuthenticatedBy -> EitherT DbError IO [Project]
list pool tenant authenticated =
  error "todo"
  {--
  Traction.runDb pool $ case tenant of
    SingleTenant ->
      case authenticated of
        AuthenticatedByDesign _ ->
          Query.getAllProjects
        AuthenticatedByGithub _session user ->
          Query.getAccountProjects (userIdOf user)
    MultiTenant ->
      case authenticated of
        AuthenticatedByDesign _ ->
          pure []
        AuthenticatedByGithub _session user ->
          Query.getAccountProjects (userIdOf user)
--}
-- FIX MTH error type
discover :: DbPool -> Tenant -> AuthenticatedBy -> ProjectName -> EitherT Text IO (Maybe BuildId)
discover pool tenant authenticated project = do
  error "todo"
  {--
  r <- firstT Traction.renderDbError $
    pick pool tenant authenticated project
  case r of
    Nothing ->
      pure Nothing
    Just _repository -> do
      i <- firstT Traction.renderDbError . Traction.runDb pool $
        Query.tick
      firstT Traction.renderDbError . Traction.runDb pool $
        Query.discover i project
      pure (Just i)
--}

data NewProjectError =
    NewProjectAlreadyExists ProjectName
  | NewProjectInvalidNameError ProjectName
  | NewProjectInvalidRepositoryError ProjectName Repository
    deriving (Eq, Ord, Show)

new :: ProjectName -> Repository -> EitherT NewProjectError Db ProjectId
new project repository = do
  unless (isValidProjectName project) $
    left $ NewProjectInvalidNameError project
  unless (isValidRepository repository) $
    left $ NewProjectInvalidRepositoryError project repository
  newEitherT $ ProjectDb.insert project repository >>= \u -> case u of
    Unique i ->
      pure . Right $ i
    Duplicate _ _ ->
      pure . Left $ NewProjectAlreadyExists project
