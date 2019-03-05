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


toBuild :: ((Int64, Text, Text) :. (Int64, Text, Maybe Text, Maybe Text, Maybe Bool, Maybe Bool, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime)) -> Keyed BuildId Build
toBuild (project :. (key, name, ref, commit, result, cancelled, queued, start, end, hearbeat)) =
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
      hearbeat)
