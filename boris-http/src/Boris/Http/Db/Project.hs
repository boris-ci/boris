{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
module Boris.Http.Db.Project (
    getAllProjects
  , getAccountProjects
  , createProject
  , importProject
  , linkProject
  , getProjectBySourceOwnerProject
  , getProjectsByOwnerProject
  , getProjectsByProject
  ) where

import           Boris.Core.Data.Project
import           Boris.Core.Data.Repository
import           Boris.Http.Data

import           P

import           Traction.Control (MonadDb)
import qualified Traction.Control as Traction
import           Traction.Sql (sql)
import qualified Traction.Sql as Traction


getAllProjects :: MonadDb m => m [Definition]
getAllProjects = do
  let q = [sql|
      SELECT p.id, p.source, p.owner_type, p.owner, p.name, p.repository
        FROM project p
    |]
  x <- Traction.query_ q
  for x $ \(i, source, otype, owner, name, repository) ->
    case (,) <$> sourceFromInt source <*> ownerTypeFromInt otype of
      Just (s, t) ->
         pure $ Definition
           (ProjectId i)
           s
           (Owner (OwnerName owner) t)
           (Project name)
           (Repository repository)
      Nothing ->
        Traction.liftDb $ Traction.failWith (Traction.DbNoResults q)

getProjectBySourceOwnerProject :: MonadDb m => Source -> OwnerName -> Project -> m (Maybe Definition)
getProjectBySourceOwnerProject sourcex owner project = do
  let q = [sql|
      SELECT p.id, p.source, p.name, p.repository, o.id, o.name, o.type
        FROM project p, owner o
       WHERE p.owner = o.id
         AND p.source = ?
         AND o.name = ?
         AND p.name = ?
    |]
  x <- Traction.unique q (sourceToInt sourcex, renderOwnerName owner, renderProject project)
  for x $ \(i, source, otype, owner, name, repository) ->
    case (,) <$> sourceFromInt source <*> ownerTypeFromInt otype of
      Just (s, t) ->
         pure $ Definition
           (ProjectId i)
           s
           (Owner (OwnerName owner) t)
           (Project name)
           (Repository repository)
      Nothing ->
        Traction.liftDb $ Traction.failWith (Traction.DbNoResults q)

getProjectsByOwnerProject :: MonadDb m => OwnerName -> Project -> m [Definition]
getProjectsByOwnerProject owner project = do
  let q = [sql|
      SELECT p.id, p.source, p.name, p.repository, o.id, o.name, o.type
        FROM project p, owner o
       WHERE p.owner = o.id
         AND o.name = ?
         AND p.name = ?
    |]
  x <- Traction.query q (renderOwnerName owner, renderProject project)
  for x $ \(i, source, otype, owner, name, repository) ->
    case (,) <$> sourceFromInt source <*> ownerTypeFromInt otype of
      Just (s, t) ->
         pure $ Definition
           (ProjectId i)
           s
           (Owner (OwnerName owner) t)
           (Project name)
           (Repository repository)
      Nothing ->
        Traction.liftDb $ Traction.failWith (Traction.DbNoResults q)

getProjectsByProject :: MonadDb m => Project -> m [Definition]
getProjectsByProject project = do
  let q = [sql|
      SELECT p.id, p.source, p.name, p.repository, o.id, o.name, o.type
        FROM project p, owner o
       WHERE p.owner = o.id
         AND p.name = ?
    |]
  x <- Traction.query q (Traction.Only $ renderProject project)
  for x $ \(i, source, otype, owner, name, repository) ->
    case (,) <$> sourceFromInt source <*> ownerTypeFromInt otype of
      Just (s, t) ->
         pure $ Definition
           (ProjectId i)
           s
           (Owner (OwnerName owner) t)
           (Project name)
           (Repository repository)
      Nothing ->
        Traction.liftDb $ Traction.failWith (Traction.DbNoResults q)

getAccountProjects :: MonadDb m => UserId -> m [Definition]
getAccountProjects account = do
  let q = [sql|
      SELECT p.id, p.source, p.name, p.repository, o.id, o.name, o.type
        FROM project p, owner o, account_projects ap
       WHERE ap.account = ?
         AND ap.project = p.id
         AND o.id = p.owner
    |]
  x <- Traction.query q (Traction.Only $ getUserId account)
  for x $ \(i, source, otype, owner, name, repository) ->
    case (,) <$> sourceFromInt source <*> ownerTypeFromInt otype of
      Just (s, t) ->
         pure $ Definition
           (ProjectId i)
           s
           (Owner (OwnerName owner) t)
           (Project name)
           (Repository repository)
      Nothing ->
        Traction.liftDb $ Traction.failWith (Traction.DbNoResults q)

createProject :: MonadDb m => OwnerType -> OwnerName -> Project -> Repository -> m ProjectId
createProject t owner project repository =
  fmap ProjectId . Traction.value $ Traction.mandatory [sql|
      INSERT INTO project (source, owner_type, owner, name, repository, enabled)
           VALUES (?, ?, ?, ?, ?, true)
        RETURNING id
    |] (sourceToInt BorisSource, ownerTypeToInt t, renderOwnerName owner, renderProject project, renderRepository repository)

createOrGetOwnedBy :: MonadDb m => OwnedBy -> m (Identified OwnedBy)
createOrGetOwnedBy o =
  case o of
   OwnedByGithubUser u -> do
     n <- (fmap . fmap) UserId . Traction.values $ Traction.unique [sql|
         SELECT o.id
           FROM owner o
          WHERE o.name = ?
            AND o.type = ?
       |] (githubLogin u, ownerTypeToInt GithubUserOwnerType)
     case n of
       Nothing ->  do
         nn <- fmap UserId . Traction.value $ Traction.mandatory [sql|
           INSERT INTO owner (name, type)
                VALUES (?, ?)
             RETURNING id
         |] (githubLogin u, ownerTypeToInt GithubUserOwnerType)
         pure $ Identified nn o
       Just nn ->
         pure $ Identified nn o
   OwnedByGithubOrganisation u -> do
     n <- (fmap . fmap) UserId . Traction.values $ Traction.unique [sql|
         SELECT o.id
           FROM owner o
          WHERE o.name = ?
            AND o.type = ?
       |] (githubName u, ownerTypeToInt GithubOrganisationOwnerType)
     case n of
       Nothing -> do
         nn <- fmap UserId . Traction.value $ Traction.mandatory [sql|
           INSERT INTO owner (name, type)
                VALUES (?, ?)
             RETURNING id
         |] (githubName u, ownerTypeToInt GithubOrganisationOwnerType)
         pure $ Identified nn o
       Just nn ->
         pure $ Identified nn o
   OwnedByBoris _ ->
     pure $ Identified (UserId 0) o

importProject :: MonadDb m => OwnedBy -> Project -> Repository -> m ProjectId
importProject owner project repository = do
  identified <- createOrGetOwnedBy owner
  exists <- (fmap . fmap) ProjectId . Traction.values $ Traction.unique [sql|
         SELECT p.id
           FROM project p
          WHERE p.source = ?
            AND p.owner = ?
            AND p.name = ?
       |] (sourceToInt GithubSource, getUserId . userIdOf $ identified, renderProject project)

  case exists of
    Nothing ->
      fmap ProjectId . Traction.value $ Traction.mandatory [sql|
          INSERT INTO project (source, owner, name, repository, enabled)
               VALUES (?, ?, ?, ?, false)
            RETURNING id
        |] (sourceToInt GithubSource, getUserId . userIdOf $ identified, renderProject project, renderRepository repository)
    Just projectId ->
      pure projectId


linkProject :: MonadDb m => ProjectId -> UserId -> Permission -> m ()
linkProject project user permission = do
  void $ Traction.execute [sql|
      INSERT INTO account_projects (account, project, permission)
           VALUES (?, ?, ?)
      ON CONFLICT (account, project)
      DO UPDATE set permission = ?
    |] (getUserId user, getProjectId project, permissionToInt permission, permissionToInt permission)
