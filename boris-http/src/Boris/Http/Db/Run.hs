{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
module Boris.Http.Db.Run (
    insert
  , setStartTime
  ) where


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
