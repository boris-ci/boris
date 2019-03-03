{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
module Boris.Http.Db.Project (
    insert
  , list
  , byId
  ) where


import           Boris.Core.Data.Project
import           Boris.Core.Data.Repository
import           Boris.Core.Data.Tenant
import           Boris.Http.Data
import           Boris.Prelude


import           Database.PostgreSQL.Simple ((:.) (..))

import           Traction.Control (MonadDb)
import qualified Traction.Control as Traction
import           Traction.QQ (sql)
import qualified Traction.Sql as Traction

insert :: MonadDb m => ProjectName -> Repository -> m ProjectId
insert p r =
  Traction.valueWith ProjectId $ Traction.mandatory [sql|
      INSERT INTO project (name, repository, enabled)
           VALUES (?, ?, true)
        RETURNING id
    |] (renderProjectName p, renderRepository r)

list :: MonadDb m => m [Keyed ProjectId Project]
list =
  (fmap . fmap) toProject $ Tranction.query_ [sql|
      SELECT id, name, repository
        FROM project
    |]

byId :: MonadDb m => ProjectId -> m (Maybe (Keyed ProjectId Project))
byId i =
  (fmap . fmap) toProject $ Tranction.unique [sql|
      SELECT id, name, repository
        FROM project
    |] (Traction.Only . getProjectId $ i)


toProject :: (Int64, Text, Text) -> Keyed ProjectId Project
toProject (i, p, r) =
  Keyed
    (ProjectId i)
    (Project
      (ProjectName p)
      (Repository r)
