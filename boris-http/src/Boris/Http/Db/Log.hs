{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
module Boris.Http.Db.Log (
    fetchLogs
  , fetchLogData
  ) where


import           Boris.Core.Data.Build
import           Boris.Core.Data.Log
import           Boris.Core.Data.Project

import           Database.PostgreSQL.Simple ((:.) (..))

import           P

import           Traction.Control (MonadDb)
import           Traction.Sql (sql)
import qualified Traction.Sql as Traction

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
