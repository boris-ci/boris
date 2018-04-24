{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
module Boris.Http.Db.Query (
    tick
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
  , demandSettings
  , getSettings
  , setSettings
  , getAllProjects
  , getAccountProjects
  , createProject
  ) where


import           Boris.Core.Data
import           Boris.Core.Data.Build
import           Boris.Core.Data.Log
import           Boris.Core.Data.Project
import           Boris.Core.Data.Repository
import           Boris.Core.Data.Tenant
import           Boris.Http.Data

import           Database.PostgreSQL.Simple ((:.) (..))

import           P

import           Traction.Control (MonadDb)
import qualified Traction.Control as Traction
import           Traction.Sql (sql)
import qualified Traction.Sql as Traction


tick :: MonadDb m => m BuildId
tick =
  fmap BuildId . Traction.value $ Traction.mandatory_ [sql|
      SELECT nextval('tick')
    |]

register :: MonadDb m => Project -> Build -> BuildId -> m ()
register project build buildid =
  void $ Traction.execute [sql|
      INSERT INTO build (build_id, project, build, queued_time)
           VALUES (?, ?, ?, now())
    |] (getBuildId buildid, renderProject project, renderBuild build)

discover :: MonadDb m => BuildId -> Project -> m ()
discover buildid project =
  void $ Traction.execute [sql|
      INSERT INTO discover (discover_id, project, queued_time)
           VALUES (?, ?, now())
    |] (getBuildId buildid, renderProject project)

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
      (Project p)
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

getProjects :: MonadDb m => Project -> m [Build]
getProjects project =
  (fmap . fmap) Build $ Traction.values $ Traction.query [sql|
      SELECT DISTINCT build
        FROM build
       WHERE project = ?
    |] (Traction.Only $ renderProject project)

getProjectCommits :: MonadDb m => Project -> m [Commit]
getProjectCommits project =
  (fmap . fmap) Commit $ Traction.values $ Traction.query [sql|
      SELECT DISTINCT commit
        FROM build
       WHERE project = ?
    |] (Traction.Only $ renderProject project)

getProjectRefs :: MonadDb m => Project -> Ref -> m [Build]
getProjectRefs project ref =
  (fmap . fmap) Build $ Traction.values $ Traction.query [sql|
      SELECT DISTINCT build
        FROM build
       WHERE project = ?
         AND ref = ?
    |] (renderProject project, renderRef ref)

getProjectCommitBuildIds :: MonadDb m => Project -> Commit -> m [BuildId]
getProjectCommitBuildIds project commit =
  (fmap . fmap) BuildId $ Traction.values $ Traction.query [sql|
      SELECT DISTINCT build_id
        FROM build
       WHERE project = ?
         AND commit = ?
    |] (renderProject project, renderCommit commit)

getProjectCommitSeen :: MonadDb m => Project -> Commit -> m [Build]
getProjectCommitSeen project commit =
  (fmap . fmap) Build $ Traction.values $ Traction.query [sql|
      SELECT DISTINCT build
        FROM build
       WHERE project = ?
         AND commit = ?
    |] (renderProject project, renderCommit commit)

getProjectCommitDiscovered :: MonadDb m => Project -> Commit -> m [Build]
getProjectCommitDiscovered project commit =
  (fmap . fmap) Build $ Traction.values $ Traction.query [sql|
      SELECT DISTINCT build
        FROM discover d, discover_commit c
       WHERE d.discover_id = c.discover_id
         AND d.project = ?
         AND c.commit = ?
    |] (renderProject project, renderCommit commit)

addProjectCommitDiscovered :: MonadDb m => BuildId -> Build -> Commit -> m ()
addProjectCommitDiscovered buildId build commit =
  void $ Traction.execute [sql|
      INSERT INTO discover_commit (discover_id, build, commit)
           VALUES (?, ?, ?)
    |] (getBuildId buildId, renderBuild build, renderCommit commit)

getBuildIds :: MonadDb m => Project -> Build -> Ref -> m [BuildId]
getBuildIds project build ref =
  (fmap . fmap) BuildId $ Traction.values $ Traction.query [sql|
      SELECT DISTINCT build_id
        FROM build
       WHERE project = ?
         AND build = ?
         AND ref = ?
    |] (renderProject project, renderBuild build, renderRef ref)

getQueued :: MonadDb m => Project -> Build -> m [BuildId]
getQueued project build =
  (fmap . fmap) BuildId $ Traction.values $ Traction.query [sql|
      SELECT DISTINCT build_id
        FROM build
       WHERE project = ?
         AND build = ?
         AND ref IS NULL
         AND build_result IS NULL
    |] (renderProject project, renderBuild build)

getBuildRefs :: MonadDb m => Project -> Build -> m [Ref]
getBuildRefs project build =
  (fmap . fmap) Ref $ Traction.values $ Traction.query [sql|
      SELECT DISTINCT ref
        FROM build
       WHERE project = ?
         AND build = ?
         AND ref IS NOT NULL
    |] (renderProject project, renderBuild build)

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
      (Project p)
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
      INSERT INTO account (github_id, github_login, github_name, github_email)
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

getSettings :: MonadDb m => m (Maybe Settings)
getSettings =
  (fmap . fmap) (bool SingleTenantSettings MultiTenantSettings) . Traction.values $ Traction.unique_ [sql|
      SELECT s.multi_tenant
        FROM settings s
    |]

demandSettings :: MonadDb m => m Settings
demandSettings =
  fmap (bool SingleTenantSettings MultiTenantSettings) . Traction.value $ Traction.mandatory_ [sql|
      SELECT s.multi_tenant
        FROM settings s
    |]

setSettings :: MonadDb m => Settings -> m ()
setSettings settings =
  getSettings >>= \s -> case s of
    Nothing ->
      void $ Traction.execute [sql|
        INSERT INTO settings (multi_tenant)
             VALUES (?)
      |] (Traction.Only $ MultiTenantSettings == settings)
    Just _ ->
      void $ Traction.execute [sql|
        UPDATE settings
           SET multi_tenant = ?
      |] (Traction.Only $ MultiTenantSettings == settings)

getAllProjects :: MonadDb m => m [Definition]
getAllProjects = do
  let q = [sql|
      SELECT p.id, p.source, p.name, p.repository, o.id, o.name, o.type
        FROM project p, owner o
    |]
  x <- Traction.query_ q
  for x $ \(i, source, name, repository, oid, oname, otype) ->
    case (,) <$> sourceFromInt source <*> ownerTypeFromInt otype of
      Just (s, t) ->
         pure $ Definition
           (ProjectId i)
           s
           (Owner (OwnerId oid) (OwnerName oname) t)
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
    |]
  x <- Traction.query q (Traction.Only $ getUserId account)
  for x $ \(i, source, name, repository, oid, oname, otype) ->
    case (,) <$> sourceFromInt source <*> ownerTypeFromInt otype of
      Just (s, t) ->
         pure $ Definition
           (ProjectId i)
           s
           (Owner (OwnerId oid) (OwnerName oname) t)
           (Project name)
           (Repository repository)
      Nothing ->
        Traction.liftDb $ Traction.failWith (Traction.DbNoResults q)

createProject :: MonadDb m => (Identified OwnedBy) -> Project -> Repository -> m ()
createProject owner project repository =
  void $ Traction.execute [sql|
      INSERT INTO project (source, owner, name, repository, enabled)
           VALUES (?, ?, ?, ?, true)
    |] (sourceToInt BorisSource, getUserId . userIdOf $ owner, renderProject project, renderRepository repository)
