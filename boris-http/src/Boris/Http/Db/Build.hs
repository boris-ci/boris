{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
module Boris.Http.Db.Build (
    insert
  , byId
  ) where


import           Boris.Core.Data.Build
import           Boris.Core.Data.Run
import           Boris.Prelude


import           Database.PostgreSQL.Simple ((:.) (..))

import           Traction.Control (MonadDb)
import qualified Traction.Control as Traction
import           Traction.QQ (sql)
import           Traction.Sql (Unique (..))
import qualified Traction.Sql as Traction

insert :: MonadDb m => RunId -> Build -> Maybe Ref -> m BuildId
insert run build ref =
  Traction.valueWith BuildId $ Traction.mandatory [sql|
      INSERT INTO build (id, build, ref)
           VALUES (?, ?, ?)
        RETURNING id
    |] (getRunId run, renderBuild build, renderRef <$> ref)


byId :: MonadDb m => BuildId -> m (Maybe BuildId)
byId build =
  Traction.valuesWith BuildId $ Traction.unique [sql|
      SELECT id
        FROM build
        WHERE id = ?
    |] (Traction.Only $ getBuildId build)
