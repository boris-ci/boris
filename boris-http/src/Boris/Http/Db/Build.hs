{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
module Boris.Http.Db.Build (
    insert
  , byId
  , byProjectId
  , byBuildName
  , refTree
  , isQueued
  , next
  , heartbeat
  , setStartTime
  , cancel
  , complete
  , index
  , built
  , toBuild
  ) where


import           Boris.Core.Data.Build
import           Boris.Core.Data.Keyed
import           Boris.Core.Data.Project
import           Boris.Core.Data.Run
import qualified Boris.Http.Db.Project as ProjectDb
import           Boris.Prelude

import           Data.Time (UTCTime)

import           Database.PostgreSQL.Simple ((:.) (..))

import           Traction.Control (MonadDb)
import           Traction.QQ (sql)
import qualified Traction.Sql as Traction

insert :: MonadDb m => RunId -> BuildName -> Maybe Ref -> m BuildId
insert run build ref =
  Traction.valueWith BuildId $ Traction.mandatory [sql|
      INSERT INTO build (id, build, ref)
           VALUES (?, ?, ?)
        RETURNING id
    |] (getRunId run, renderBuildName build, renderRef <$> ref)


byId :: MonadDb m => BuildId -> m (Maybe (Keyed BuildId Build))
byId build =
  (fmap . fmap) toBuild $ Traction.unique [sql|
      SELECT p.id, p.name, p.repository, b.id, b.build, b.ref, b.commit, b.build_result, r.cancelled, r.queued_time,
             r.start_time, r.end_time, r.heartbeat_time
        FROM build b
        JOIN run r
          ON r.id = b.id
         AND r.run_type = ?
        JOIN project p
          ON r.project = p.id
       WHERE b.id = ?
    |] (runTypeToInt IsBuild, getBuildId build)

byProjectId :: MonadDb m => ProjectId -> m [Keyed BuildId Build]
byProjectId project =
  (fmap . fmap) toBuild $ Traction.query [sql|
      SELECT p.id, p.name, p.repository, b.id, b.build, b.ref, b.commit, b.build_result, r.cancelled, r.queued_time,
             r.start_time, r.end_time, r.heartbeat_time
        FROM build b
        JOIN run r
          ON r.id = b.id
         AND r.run_type = ?
        JOIN project p
          ON r.project = p.id
       WHERE p.id = ?
    |] (runTypeToInt IsBuild, getProjectId project)

byBuildName :: MonadDb m => ProjectName -> BuildName -> m [Keyed BuildId Build]
byBuildName project  build=
  (fmap . fmap) toBuild $ Traction.query [sql|
      SELECT p.id, p.name, p.repository, b.id, b.build, b.ref, b.commit, b.build_result, r.cancelled, r.queued_time,
             r.start_time, r.end_time, r.heartbeat_time
        FROM build b
        JOIN run r
          ON r.id = b.id
         AND r.run_type = ?
        JOIN project p
          ON r.project = p.id
       WHERE p.name = ?
         AND b.name = ?
    |] (runTypeToInt IsBuild, renderProjectName project, renderBuildName build)

isQueued :: MonadDb m => ProjectName -> BuildName -> m [Keyed BuildId Build]
isQueued project build=
  (fmap . fmap) toBuild $ Traction.query [sql|
      SELECT p.id, p.name, p.repository, b.id, b.build, b.ref, b.commit, b.build_result, r.cancelled, r.queued_time,
             r.start_time, r.end_time, r.heartbeat_time
        FROM build b
        JOIN run r
          ON r.id = b.id
         AND r.run_type = ?
        JOIN project p
          ON r.project = p.id
       WHERE p.name = ?
         AND b.build = ?
         AND b.build_result IS NULL
         AND r.start_time IS NULL
    |] (runTypeToInt IsBuild, renderProjectName project, renderBuildName build)

-- TODO locking, different table for queue, invisibility timeout
next :: MonadDb m => m (Maybe (Keyed BuildId Build))
next =
  (fmap . fmap) toBuild $ Traction.unique_ [sql|
      SELECT p.id, p.name, p.repository, b.id, b.build, b.ref, b.commit, b.build_result, r.cancelled, r.queued_time,
             r.start_time, r.end_time, r.heartbeat_time
        FROM build b
        JOIN run r
          ON r.id = b.id
        JOIN project p
          ON r.project = p.id
       WHERE b.build_result IS NULL
         AND r.start_time IS NULL
       LIMIT 1
    |]

refTree :: MonadDb m => ProjectName -> BuildName -> m BuildTree
refTree project build = do
  refs <- Traction.valuesWith Ref $ Traction.query [sql|
      SELECT DISTINCT b.ref
        FROM build b
        JOIN run r
          ON r.id = b.id
         AND r.run_type = ?
        JOIN project p
          ON r.project = p.id
       WHERE p.name = ?
         AND b.build = ?
         AND b.ref IS NOT NULL
    |] (runTypeToInt IsBuild, renderProjectName project, renderBuildName build)

  fmap (BuildTree project build) . for refs $ \ref ->
    fmap (BuildTreeRef ref) . Traction.valuesWith BuildId $ Traction.query [sql|
        SELECT b.id
          FROM build b
          JOIN run r
            ON r.id = b.id
           AND r.run_type = ?
          JOIN project p
            ON r.project = p.id
         WHERE p.name = ?
           AND b.build = ?
           AND b.ref = ?
      |] (runTypeToInt IsBuild, renderProjectName project, renderBuildName build, renderRef ref)

-- TODO All of this stuff should be on Run

heartbeat :: MonadDb m => BuildId -> m BuildCancelled
heartbeat buildid =
  fmap (fromMaybe BuildNotCancelled) . (fmap . fmap) (bool BuildNotCancelled BuildCancelled) . fmap join . Traction.values $ Traction.unique [sql|
          UPDATE run
             SET heartbeat_time = now()
           WHERE id = ?
       RETURNING cancelled
    |] (Traction.Only $ getBuildId buildid)

setStartTime :: MonadDb m => BuildId -> m ()
setStartTime buildId =
  void $ Traction.execute [sql|
          UPDATE run
             SET start_time = now()
           WHERE id = ?
    |] (Traction.Only $ getBuildId buildId)

cancel :: MonadDb m => BuildId -> m Bool
cancel buildid = do
  cancelled <- fmap (> 0) $ Traction.execute [sql|
      UPDATE run
         SET cancelled = true,
             end_time = now()
       WHERE id = ?
         AND cancelled IS NULL
    |] (Traction.Only . getBuildId $ buildid)
  when cancelled $
    void $ Traction.execute [sql|
        UPDATE build
           SET build_result = false
         WHERE id = ?
      |] (Traction.Only . getBuildId $ buildid)
  pure cancelled

complete :: MonadDb m => BuildId -> BuildResult -> m Bool
complete buildid result = do
  completed <- fmap (> 0) $ Traction.execute [sql|
      UPDATE run
         SET end_time = now()
       WHERE id = ?
    |] (Traction.Only . getBuildId $ buildid)

  when completed $
    void $ Traction.execute [sql|
          UPDATE build
             SET build_result = ?
           WHERE id = ?
    |] (case result of BuildOk -> True; BuildKo -> False, getBuildId buildid)

  pure completed

index :: MonadDb m => BuildId -> Ref -> Commit -> m ()
index buildid ref commit =
  void $ Traction.execute [sql|
      UPDATE build
         SET ref = ?,
             commit = ?
       WHERE id = ?
    |] (renderRef ref, renderCommit commit, getBuildId buildid)

built :: MonadDb m => ProjectId -> Commit -> m [BuildName]
built project commit =
  Traction.valuesWith BuildName $ Traction.query [sql|
      SELECT DISTINCT b.build
        FROM build b
        JOIN run r
          on r.id = b.id
       WHERE r.project = ?
         AND b.commit = ?
    |] (getProjectId project, renderCommit commit)

toBuild :: ((Int64, Text, Text) :. (Int64, Text, Maybe Text, Maybe Text, Maybe Bool, Maybe Bool, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime)) -> Keyed BuildId Build
toBuild (project :. (key, name, ref, commit, result, cancelled, queued, start, end, heartbeatx)) =
  Keyed
    (BuildId key)
    (Build
      (ProjectDb.toProject project)
      (BuildName name)
      (Ref <$> ref)
      (Commit <$> commit)
      (bool BuildKo BuildOk <$> result)
      (bool BuildCancelled BuildNotCancelled <$> cancelled)
      queued
      start
      end
      heartbeatx)
