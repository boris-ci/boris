{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
module Boris.Http.Db.Run (
    insert
  , setStartTime
  , heartbeat
  , cancel
  ) where


import           Boris.Core.Data.Build
import           Boris.Core.Data.Project
import           Boris.Core.Data.Run
import           Boris.Prelude


import           Database.PostgreSQL.Simple ((:.) (..))

import           Traction.Control (MonadDb)
import qualified Traction.Control as Traction
import           Traction.QQ (sql)
import           Traction.Sql (Unique (..))
import qualified Traction.Sql as Traction

insert :: MonadDb m => RunType -> ProjectId -> m RunId
insert r p =
  Traction.valueWith RunId $ Traction.mandatory [sql|
      INSERT INTO run (run_type, project)
           VALUES (?, ?)
        RETURNING id
    |] (runTypeToInt r, getProjectId p)

setStartTime :: MonadDb m => RunId -> m ()
setStartTime run =
  void $ Traction.execute [sql|
          UPDATE run
             SET start_time = now()
           WHERE id = ?
    |] (Traction.Only $ getRunId run)


-- TODO BuildCancelled -> RunCancelled
heartbeat :: MonadDb m => RunId -> m BuildCancelled
heartbeat run =
  fmap (fromMaybe BuildNotCancelled) . (fmap . fmap) (bool BuildNotCancelled BuildCancelled) . fmap join . Traction.values $ Traction.unique [sql|
          UPDATE run
             SET heartbeat_time = now()
           WHERE id = ?
       RETURNING cancelled
    |] (Traction.Only $ getRunId run)

cancel :: MonadDb m => RunId -> m Bool
cancel run = do
  cancelled <- fmap (> 0) $ Traction.execute [sql|
      UPDATE run
         SET cancelled = true,
             end_time = now()
       WHERE id = ?
         AND cancelled IS NULL
    |] (Traction.Only . getRunId $ run)
  when cancelled $
    void $ Traction.execute [sql|
        UPDATE build
           SET build_result = false
         WHERE id = ?
      |] (Traction.Only . getRunId $ run)
  pure cancelled
