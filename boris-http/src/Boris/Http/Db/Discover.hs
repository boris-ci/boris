{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
module Boris.Http.Db.Discover (
    discover
  , getProjectCommitDiscovered
  , addProjectCommitDiscovered
  ) where


import           Boris.Core.Data.Build
import           Boris.Core.Data.Project

import           P

import           Traction.Control (MonadDb)
import           Traction.Sql (sql)
import qualified Traction.Sql as Traction


discover :: MonadDb m => BuildId -> Project -> m ()
discover buildid project =
  void $ Traction.execute [sql|
      INSERT INTO discover (discover_id, project, queued_time)
           VALUES (?, ?, now())
    |] (getBuildId buildid, renderProject project)

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
