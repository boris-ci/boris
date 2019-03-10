{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
module Boris.Http.Db.Discover (
    insert
  , byId
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
import qualified Traction.Sql as Traction

insert :: MonadDb m => RunId -> m DiscoverId
insert run =
  Traction.valueWith DiscoverId $ Traction.mandatory [sql|
      INSERT INTO discover (id)
           VALUES (?)
        RETURNING id
    |] (Traction.Only $ getRunId run)

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
