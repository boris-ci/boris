{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
module Boris.Http.Db.Project (
    insert
  , list
  , byId
  , byName
  , toProject
  ) where


import           Boris.Core.Data.Keyed
import           Boris.Core.Data.Project
import           Boris.Core.Data.Repository
import           Boris.Prelude


import           Database.PostgreSQL.Simple ((:.) (..))

import           Traction.Control (MonadDb)
import qualified Traction.Control as Traction
import           Traction.QQ (sql)
import           Traction.Sql (Unique (..))
import qualified Traction.Sql as Traction

insert :: MonadDb m => ProjectName -> Repository -> m (Unique ProjectId)
insert p r =
  Traction.withUniqueCheck . Traction.valueWith ProjectId $ Traction.mandatory [sql|
      INSERT INTO project (name, repository, enabled)
           VALUES (?, ?, true)
        RETURNING id
    |] (renderProjectName p, renderRepository r)

list :: MonadDb m => m [Keyed ProjectId Project]
list =
  (fmap . fmap) toProject $ Traction.query_ [sql|
      SELECT id, name, repository
        FROM project
    |]

byId :: MonadDb m => ProjectId -> m (Maybe (Keyed ProjectId Project))
byId i =
  (fmap . fmap) toProject $ Traction.unique [sql|
      SELECT id, name, repository
        FROM project
       WHERE id = ?
    |] (Traction.Only . getProjectId $ i)

byName :: MonadDb m => ProjectName -> m (Maybe (Keyed ProjectId Project))
byName p =
  (fmap . fmap) toProject $ Traction.unique [sql|
      SELECT id, name, repository
        FROM project
       WHERE name = ?
    |] (Traction.Only . renderProjectName $ p)


toProject :: (Int64, Text, Text) -> Keyed ProjectId Project
toProject (i, p, r) =
  Keyed
    (ProjectId i)
    (Project
      (ProjectName p)
      (Repository r))
