{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Project (
    picker

  , ProjectReferenceResolution (..)
  , byReference
  , pick
  , list
  , new

  , ProjectReferenceResolveError (..)
  , renderProjectReferenceResolveError
  ) where

import           Boris.Core.Data.Project
import           Boris.Core.Data.Repository
import           Boris.Core.Data.Tenant
import           Boris.Http.Data
import qualified Boris.Http.Db.Project as ProjectDb

import qualified Data.List as List
import qualified Data.Text as Text

import           P

import           System.IO (IO)

import           Traction.Control (DbPool, DbError)
import qualified Traction.Control as Traction

import           X.Control.Monad.Trans.Either (EitherT)

picker :: Project -> [Definition] -> Maybe Repository
picker project =
  fmap definitionRepository . List.find ((==) project . definitionProject)

pick :: DbPool -> Tenant -> AuthenticatedBy -> Project -> EitherT DbError IO (Maybe Repository)
pick pool tenant authenticated project =
  Traction.runDb pool $ case tenant of
    SingleTenant ->
      case authenticated of
        AuthenticatedByDesign _ ->
          picker project <$> ProjectDb.getAllProjects
        AuthenticatedByGithub _session user ->
           picker project <$> ProjectDb.getAccountProjects (userIdOf user)
    MultiTenant ->
      case authenticated of
        AuthenticatedByDesign _ ->
          pure Nothing
        AuthenticatedByGithub _session user ->
          picker project <$> ProjectDb.getAccountProjects (userIdOf user)

data ProjectReferenceResolveError =
    ProjectReferenceResolveDbError DbError
    deriving Show

renderProjectReferenceResolveError :: ProjectReferenceResolveError -> Text
renderProjectReferenceResolveError err =
  case err of
    ProjectReferenceResolveDbError e ->
      mconcat ["Project reference resolve error via db: ", Traction.renderDbError e]

data ProjectReferenceResolution =
    NoProjectReferenceResolution
  | ExactProjectReferenceResolution Definition
  | QualifiedProjectReferenceResolution Definition
  | AmbiguousProjectReferenceResolution [Definition]
  | InvalidProjectReferenceResolution
    deriving (Eq, Show)

byReference :: DbPool -> ProjectReference -> EitherT ProjectReferenceResolveError IO ProjectReferenceResolution
byReference pool ref =
  case Text.splitOn ":" $ renderProjectReference ref of
    [project] -> do
      candidates <- firstT ProjectReferenceResolveDbError . Traction.runDb pool $
        ProjectDb.getProjectsByProject (Project project)
      case candidates of
        [exact] ->
          pure $ ExactProjectReferenceResolution exact
        [] ->
          pure $ NoProjectReferenceResolution
        _ ->
          pure $ AmbiguousProjectReferenceResolution candidates
    [owner, project] -> do
      candidates <- firstT ProjectReferenceResolveDbError . Traction.runDb pool $
        ProjectDb.getProjectsByOwnerProject (OwnerName owner) (Project project)
      case candidates of
        [exact] ->
          pure $ ExactProjectReferenceResolution exact
        [] ->
          pure $ NoProjectReferenceResolution
        _ ->
          pure $ AmbiguousProjectReferenceResolution candidates
    ["github", owner, project] -> do
      fmap (maybe NoProjectReferenceResolution QualifiedProjectReferenceResolution) . firstT ProjectReferenceResolveDbError . Traction.runDb pool $
        ProjectDb.getProjectBySourceOwnerProject GithubSource (OwnerName owner) (Project project)
    ["boris", owner, project] -> do
      fmap (maybe NoProjectReferenceResolution QualifiedProjectReferenceResolution) . firstT ProjectReferenceResolveDbError . Traction.runDb pool $
        ProjectDb.getProjectBySourceOwnerProject BorisSource (OwnerName owner) (Project project)
    _ ->
      pure InvalidProjectReferenceResolution

list :: DbPool -> Tenant -> AuthenticatedBy -> EitherT DbError IO [Definition]
list pool tenant authenticated =
  Traction.runDb pool $ case tenant of
    SingleTenant ->
      case authenticated of
        AuthenticatedByDesign _ ->
          ProjectDb.getAllProjects
        AuthenticatedByGithub _session user ->
          ProjectDb.getAccountProjects (userIdOf user)
    MultiTenant ->
      case authenticated of
        AuthenticatedByDesign _ ->
          pure []
        AuthenticatedByGithub _session user ->
          ProjectDb.getAccountProjects (userIdOf user)

new :: DbPool -> AuthenticatedBy -> Project -> Repository -> EitherT DbError IO ()
new pool authenticated project repository =
  Traction.runDb pool $
    case authenticated of
      AuthenticatedByGithub _ u ->
        ProjectDb.createProject (OwnedByGithubUser <$> fmap githubUserLogin u) project repository
      AuthenticatedByDesign u ->
        ProjectDb.createProject (OwnedByBoris <$> u) project repository
