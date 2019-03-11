{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
module Boris.Http.Db.Discover (
    insert
  , byId
  , add
  , discovered
  , toDiscover
  ) where


import           Boris.Core.Data.Build
import           Boris.Core.Data.Discover
import           Boris.Core.Data.Keyed
import           Boris.Core.Data.Project
import           Boris.Core.Data.Run
import qualified Boris.Http.Db.Project as ProjectDb
import           Boris.Prelude

import           Data.Time (UTCTime)

import           Database.PostgreSQL.Simple ((:.) (..))

import           Traction.Control (MonadDb)
import           Traction.QQ (sql)
import           Traction.Sql (Unique)
import qualified Traction.Sql as Traction

insert :: MonadDb m => RunId -> m DiscoverId
insert run =
  Traction.valueWith DiscoverId $ Traction.mandatory [sql|
      INSERT INTO discover (id)
           VALUES (?)
        RETURNING id
    |] (Traction.Only $ getRunId run)

discovered :: MonadDb m => ProjectId -> Commit -> m [BuildName]
discovered project commit =
  Traction.valuesWith BuildName $ Traction.query [sql|
      SELECT DISTINCT c.build
        FROM discover d
        JOIN run r
          on r.id = d.id
        JOIN discover_commit c
          ON d.id = c.discover_id
       WHERE r.project = ?
         AND c.commit = ?
    |] (getProjectId project, renderCommit commit)

add :: MonadDb m => DiscoverId -> BuildName -> Commit -> m (Unique ())
add discover build commit =
  Traction.withUniqueCheck . void $ Traction.execute [sql|
      INSERT INTO discover_commit(discover_id, build, commit)
           VALUES (?, ?, ?)
    |] (getDiscoverId discover, renderBuildName build, renderCommit commit)

byId :: MonadDb m => DiscoverId -> m (Maybe (Keyed DiscoverId Discover))
byId discover =
  (fmap . fmap) toDiscover $ Traction.unique [sql|
      SELECT p.id, p.name, p.repository, d.id, r.cancelled, r.queued_time,
             r.start_time, r.end_time, r.heartbeat_time
        FROM discover d
        JOIN run r
          ON r.id = d.id
         AND r.run_type = ?
        JOIN project p
          ON r.project = p.id
       WHERE d.id = ?
    |] (runTypeToInt IsDiscover, getDiscoverId discover)


toDiscover :: ((Int64, Text, Text) :. (Int64, Maybe Bool, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime)) -> Keyed DiscoverId Discover
toDiscover (project :. (key, cancelled, queued, start, end, heartbeatx)) =
  Keyed
    (DiscoverId key)
    (Discover
      (ProjectDb.toProject project)
      (bool BuildCancelled BuildNotCancelled <$> cancelled)
      queued
      start
      end
      heartbeatx)
