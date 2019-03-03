{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
module Boris.Http.Db.Query (
{--    tick
  , fetch
  , fetchLogs
  , fetchLogData
  , cancel
  , register
  , acknowledge
  , complete
  , heartbeat
  , index
  , results
  , getProjects
  , getProjectCommits
  , getProjectRefs
  , getProjectCommitBuildIds
  , getProjectCommitSeen
  , getProjectCommitDiscovered
  , addProjectCommitDiscovered
  , getBuildIds
  , getQueued
  , getBuildRefs
  , discover
  , userByGithubId
  , addUser
  , updateUser
  , newSession
  , tickSession
  , getSession
  , getSessionUser
  , getSessionOAuth
  , demandTenant
  , getTenant
  , setTenant--}
  ) where


import           Boris.Core.Data.Build
import           Boris.Core.Data.Log
import           Boris.Core.Data.Project
import           Boris.Core.Data.Repository
import           Boris.Core.Data.Tenant
import           Boris.Http.Data
import           Boris.Prelude

import           Database.PostgreSQL.Simple ((:.) (..))

import           Traction.Control (MonadDb)
import qualified Traction.Control as Traction
import           Traction.QQ (sql)
import qualified Traction.Sql as Traction

{--
tick :: MonadDb m => m BuildId
tick =
  fmap BuildId . Traction.value $ Traction.mandatory_ [sql|
      SELECT nextval('tick')
    |]

register :: MonadDb m => ProjectId -> Build -> BuildId -> m ()
register project build buildid =
  void $ Traction.execute [sql|
      INSERT INTO build (build_id, project, build, queued_time)
           VALUES (?, ?, ?, now())
    |] (getBuildId buildid, getProjectId project, renderBuild build)

discover :: MonadDb m => BuildId -> ProjectName -> m ()
discover buildid project =
  void $ Traction.execute [sql|
      INSERT INTO discover (discover_id, project, queued_time)
           VALUES (?, ?, now())
    |] (getBuildId buildid, renderProjectName project)

fetch :: MonadDb m => BuildId -> m (Maybe BuildData)
fetch i = do
  x <- Traction.unique [sql|
      SELECT project, build, ref, commit, queued_time,
             start_time, end_time, heartbeat_time, build_result,
             cancelled
        FROM build
       WHERE build_id = ?
    |] (Traction.Only $ getBuildId i)
  pure . with x $ \((p, b, r, c, qt, st, et, ht) :. (br, cancelled)) ->
    BuildData
      i
      (ProjectName p)
      (Build b)
      (Ref <$> r)
      (Commit <$> c)
      qt
      st
      et
      ht
      (bool BuildKo BuildOk <$> br)
      (bool BuildNotCancelled BuildCancelled <$> cancelled)

fetchLogData :: MonadDb m => BuildId -> m LogData
fetchLogData build =
  fmap DBLog $
    fetchLogs build

fetchLogs :: MonadDb m => BuildId -> m [DBLogData]
fetchLogs i = do
  x <- Traction.query [sql|
      SELECT logged_at, log_payload
        FROM log
       WHERE build_id = ?
    |] (Traction.Only $ getBuildId i)
  pure . with x $ \(tm, tt) ->
    DBLogData
      tm
      tt

getProjects :: MonadDb m => ProjectName -> m [Build]
getProjects project =
  (fmap . fmap) Build $ Traction.values $ Traction.query [sql|
      SELECT DISTINCT build
        FROM build b
        JOIN project p
          ON b.project = p.id
         AND p.name = ?
    |] (Traction.Only $ renderProjectName project)

getProjectCommits :: MonadDb m => ProjectName -> m [Commit]
getProjectCommits project =
  (fmap . fmap) Commit $ Traction.values $ Traction.query [sql|
      SELECT DISTINCT commit
        FROM build
       WHERE project = ?
    |] (Traction.Only $ renderProjectName project)

getProjectRefs :: MonadDb m => ProjectName -> Ref -> m [Build]
getProjectRefs project ref =
  (fmap . fmap) Build $ Traction.values $ Traction.query [sql|
      SELECT DISTINCT build
        FROM build
       WHERE project = ?
         AND ref = ?
    |] (renderProjectName project, renderRef ref)

getProjectCommitBuildIds :: MonadDb m => ProjectName -> Commit -> m [BuildId]
getProjectCommitBuildIds project commit =
  (fmap . fmap) BuildId $ Traction.values $ Traction.query [sql|
      SELECT DISTINCT build_id
        FROM build
       WHERE project = ?
         AND commit = ?
    |] (renderProjectName project, renderCommit commit)

getProjectCommitSeen :: MonadDb m => ProjectName -> Commit -> m [Build]
getProjectCommitSeen project commit =
  (fmap . fmap) Build $ Traction.values $ Traction.query [sql|
      SELECT DISTINCT build
        FROM build
       WHERE project = ?
         AND commit = ?
    |] (renderProjectName project, renderCommit commit)

getProjectCommitDiscovered :: MonadDb m => ProjectName -> Commit -> m [Build]
getProjectCommitDiscovered project commit =
  (fmap . fmap) Build $ Traction.values $ Traction.query [sql|
      SELECT DISTINCT build
        FROM discover d, discover_commit c
       WHERE d.discover_id = c.discover_id
         AND d.project = ?
         AND c.commit = ?
    |] (renderProjectName project, renderCommit commit)

addProjectCommitDiscovered :: MonadDb m => BuildId -> Build -> Commit -> m ()
addProjectCommitDiscovered buildId build commit =
  void $ Traction.execute [sql|
      INSERT INTO discover_commit (discover_id, build, commit)
           VALUES (?, ?, ?)
    |] (getBuildId buildId, renderBuild build, renderCommit commit)

getBuildIds :: MonadDb m => ProjectName -> Build -> Ref -> m [BuildId]
getBuildIds project build ref =
  (fmap . fmap) BuildId $ Traction.values $ Traction.query [sql|
      SELECT DISTINCT build_id
        FROM build
       WHERE project = ?
         AND build = ?
         AND ref = ?
    |] (renderProjectName project, renderBuild build, renderRef ref)

getQueued :: MonadDb m => ProjectName -> Build -> m [BuildId]
getQueued project build =
  (fmap . fmap) BuildId $ Traction.values $ Traction.query [sql|
      SELECT DISTINCT build_id
        FROM build
       WHERE project = ?
         AND build = ?
         AND ref IS NULL
         AND build_result IS NULL
    |] (renderProjectName project, renderBuild build)

getBuildRefs :: MonadDb m => ProjectName -> Build -> m [Ref]
getBuildRefs project build =
  (fmap . fmap) Ref $ Traction.values $ Traction.query [sql|
      SELECT DISTINCT ref
        FROM build
       WHERE project = ?
         AND build = ?
         AND ref IS NOT NULL
    |] (renderProjectName project, renderBuild build)

cancel :: MonadDb m => BuildId -> m ()
cancel buildid =
  void $ Traction.execute [sql|
      UPDATE build
         SET cancelled = true,
             build_result = false,
             end_time = now()
       WHERE build_id = ?
         AND cancelled IS NULL
    |] (Traction.Only . getBuildId $ buildid)

acknowledge :: MonadDb m => BuildId -> m Acknowledge
acknowledge buildid =
  fmap (bool Accept AlreadyRunning . (==) (0 :: Int64)) $ Traction.execute [sql|
          UPDATE build
             SET start_time = now()
           WHERE build_id = ?
             AND start_time IS NULL
    |] (Traction.Only $ getBuildId buildid)

complete :: MonadDb m => BuildId -> BuildResult -> m (Maybe Ref)
complete buildid result =
  (fmap . fmap) Ref . fmap join . Traction.values $ Traction.unique [sql|
          UPDATE build
             SET end_time = now(),
                 build_result = ?
           WHERE build_id = ?
       RETURNING ref
    |] (case result of BuildOk -> True; BuildKo -> False, getBuildId buildid)

heartbeat :: MonadDb m => BuildId -> m BuildCancelled
heartbeat buildid =
  fmap (fromMaybe BuildNotCancelled) . (fmap . fmap) (bool BuildNotCancelled BuildCancelled) . fmap join . Traction.values $ Traction.unique [sql|
          UPDATE build
             SET heartbeat_time = now()
           WHERE build_id = ?
       RETURNING cancelled
    |] (Traction.Only $ getBuildId buildid)

index :: MonadDb m => BuildId -> Ref -> Commit -> m ()
index buildid ref commit =
  void $ Traction.execute [sql|
      UPDATE build
         SET ref = ?,
             commit = ?
       WHERE build_id = ?
    |] (renderRef ref, renderCommit commit, getBuildId buildid)

results :: MonadDb m => m [Result]
results = do
  rs <- Traction.query_ [sql|
      SELECT build_id, project, build, ref, build_result
        FROM build
       WHERE build_id IN (
             SELECT max(build_id)
               FROM build
              WHERE build_result IS NOT NULL
                AND ref IS NOT NULL
                AND ref = 'refs/heads/master'
              GROUP BY project, build, ref)
    |]
  pure . with rs $ \(i, p, b, r, br) ->
    Result
      (BuildId i)
      (ProjectName p)
      (Build b)
      (Ref <$> r)
      (bool BuildKo BuildOk br)

userByGithubId :: MonadDb m => GithubId -> m (Maybe (Identified GithubUser))
userByGithubId uid = do
  x <- Traction.unique [sql|
      SELECT id, github_id, github_login, github_name, github_email
        FROM account
       WHERE github_id = ?
    |] (Traction.Only $ githubId uid)
  pure . with x $ \(i, guid, login, name, email) ->
    Identified
      (UserId i)
      (GithubUser
        (GithubId guid)
        (GithubLogin login)
        (GithubName <$> name)
        (GithubEmail <$> email))

updateUser :: MonadDb m => Identified GithubUser -> m ()
updateUser user = do
  void $ Traction.execute [sql|
      UPDATE account
         SET github_id = ?
           , github_login = ?
           , github_name = ?
           , github_email = ?
           , updated = now()
       WHERE id = ?
    |] (githubId . githubUserId . userOf $ user
      , githubLogin . githubUserLogin . userOf $ user
      , fmap githubName . githubUserName. userOf $ user
      , fmap githubEmail . githubUserEmail . userOf $ user
      , getUserId . userIdOf $ user)

addUser :: MonadDb m => GithubUser -> m (Identified GithubUser)
addUser user = do
  i <- Traction.value $ Traction.mandatory [sql|
      INSERT INTO github_account (github_id, github_login, github_name, github_email)
           VALUES (?, ?, ?, ?)
        RETURNING id
    |] (githubId . githubUserId $ user
      , githubLogin . githubUserLogin $ user
      , fmap githubName . githubUserName $ user
      , fmap githubEmail . githubUserEmail $ user)
  pure $ Identified (UserId i) user


newSession :: MonadDb m => Session -> UserId -> m ()
newSession session user = do
  void $ Traction.execute [sql|
      INSERT INTO session (id, account, oauth)
           VALUES (?, ?, ?)
    |] (getSessionId . sessionIdentifier $ session
      , getUserId user
      , githubOAuth . sessionOAuth $ session)

tickSession :: MonadDb m => SessionId -> m ()
tickSession session = do
  void $ Traction.execute [sql|
      UPDATE session
         SET updated = now()
       WHERE id = ?
    |] (Traction.Only . getSessionId $ session)

getSessionUser :: MonadDb m => SessionId -> m (Maybe UserId)
getSessionUser session = do
  (fmap . fmap) UserId . Traction.values $ Traction.unique [sql|
      SELECT account
        FROM session
       WHERE id = ?
         AND updated > now() - INTERVAL '1 day'
    |] (Traction.Only $ getSessionId $ session)

getSessionOAuth :: MonadDb m => SessionId -> m (Maybe GithubOAuth)
getSessionOAuth session = do
  (fmap . fmap) GithubOAuth . Traction.values $ Traction.unique [sql|
      SELECT oauth
        FROM session
       WHERE id = ?
         AND updated > now() - INTERVAL '1 day'
    |] (Traction.Only $ getSessionId $ session)


getSession :: MonadDb m => SessionId -> m (Maybe AuthenticatedUser)
getSession session = do
  x <- Traction.unique [sql|
      SELECT s.account, s.oauth, a.github_id, a.github_login, a.github_name, a.github_email
        FROM session s
        JOIN account a ON s.account = a.id
       WHERE s.id = ?
         AND s.updated > now() - INTERVAL '1 day'
    |] (Traction.Only $ getSessionId $ session)
  pure . with x $ \(i, oauth, guid, login, name, email) ->
    AuthenticatedUser
      (Identified
        (UserId i)
        (GithubUser
          (GithubId guid)
          (GithubLogin login)
          (GithubName <$> name)
          (GithubEmail <$> email)))
      (Session session $ GithubOAuth oauth)

getTenant :: MonadDb m => m (Maybe Tenant)
getTenant =
  (fmap . fmap) (bool SingleTenant MultiTenant) . Traction.values $ Traction.unique_ [sql|
      SELECT s.multi_tenant
        FROM settings s
    |]

demandTenant :: MonadDb m => m Tenant
demandTenant =
  fmap (bool SingleTenant MultiTenant) . Traction.value $ Traction.mandatory_ [sql|
      SELECT s.multi_tenant
        FROM settings s
    |]

setTenant :: MonadDb m => Tenant -> m ()
setTenant settings =
  getTenant >>= \s -> case s of
    Nothing ->
      void $ Traction.execute [sql|
        INSERT INTO settings (multi_tenant)
             VALUES (?)
      |] (Traction.Only $ MultiTenant == settings)
    Just _ ->
      void $ Traction.execute [sql|
        UPDATE settings
           SET multi_tenant = ?
      |] (Traction.Only $ MultiTenant == settings)

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
--}
