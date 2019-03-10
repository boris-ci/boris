{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
module Boris.Http.Db.Queue (
    insert
  , next
  , acknowledge
  ) where


import           Boris.Core.Data.Build
import           Boris.Core.Data.Discover
import           Boris.Core.Data.Keyed
import           Boris.Core.Data.Run
import qualified Boris.Http.Db.Build as BuildDb
import qualified Boris.Http.Db.Discover as DiscoverDb
import           Boris.Prelude


import           Database.PostgreSQL.Simple (Query)

import           Traction.Control (MonadDb)
import qualified Traction.Control as Traction
import           Traction.QQ (sql)
import qualified Traction.Sql as Traction

insert :: MonadDb m => RunId -> RunType -> m ()
insert r t =
  void $ Traction.execute [sql|
      INSERT INTO queue (id, run_type)
           VALUES (?, ?)
    |] (getRunId r, runTypeToInt t)

next :: MonadDb m => m (Maybe (Either (Keyed DiscoverId Discover) (Keyed BuildId Build)))
next = do
  let
    q = [sql|
        UPDATE queue
           SET last_read = now()
         WHERE id IN (
          SELECT q.id
            FROM queue q
           WHERE last_read IS NULL OR last_read < now() - INTERVAL '15 seconds'
           ORDER BY q.id
           LIMIT 1)
        RETURNING id, run_type
      |]
  r <- Traction.unique_ q >>= mapM (toQueued q)
  fmap join . for r $ \(run, t)->
    case t of
      IsDiscover ->
        (fmap . fmap) Left $ DiscoverDb.byId (DiscoverId . getRunId $ run)
      IsBuild ->
        (fmap . fmap) Right $ BuildDb.byId (BuildId . getRunId $ run)

acknowledge :: MonadDb m => RunId -> m Acknowledge
acknowledge r =
  fmap (bool Accept AlreadyRunning . (==) (0 :: Int64)) $ Traction.execute [sql|
          DELETE
            FROM queue
           WHERE id = ?
    |] (Traction.Only $ getRunId r)

toQueued :: MonadDb m => Query -> (Int64, Int64) -> m (RunId, RunType)
toQueued q (k, t) =
  case runTypeFromInt t of
    Nothing ->
      Traction.liftDb . Traction.failWith $
        Traction.DbEncodingInvariant q "run_type" "RunType"
    Just r ->
      pure (RunId k, r)
