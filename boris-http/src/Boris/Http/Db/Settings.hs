{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
module Boris.Http.Db.Settings (
    demandTenant
  , getTenant
  , setTenant
  ) where


import           Boris.Core.Data.Build
import           Boris.Core.Data.Log
import           Boris.Core.Data.Project
import           Boris.Core.Data.Repository
import           Boris.Core.Data.Tenant
import           Boris.Http.Data

import           Database.PostgreSQL.Simple ((:.) (..))

import           P

import           Traction.Control (MonadDb)
import qualified Traction.Control as Traction
import           Traction.Sql (sql)
import qualified Traction.Sql as Traction

getTenant :: MonadDb m => m (Maybe Tenant)
getTenant =
  (fmap . fmap) (bool SingleTenant MultiTenant) . Traction.values $ Traction.unique_ [sql|
      SELECT s.multi_tenant
        FROM settings s
    |]

demandTenant :: MonadDb m => m Tenant
demandTenant =
  fmap (bool SingleTenant MultiTenant) . Traction.value $ Traction.mandatory_ [sql|
      SELECT s.multi_tenant
        FROM settings s
    |]

setTenant :: MonadDb m => Tenant -> m ()
setTenant settings =
  getTenant >>= \s -> case s of
    Nothing ->
      void $ Traction.execute [sql|
        INSERT INTO settings (multi_tenant)
             VALUES (?)
      |] (Traction.Only $ MultiTenant == settings)
    Just _ ->
      void $ Traction.execute [sql|
        UPDATE settings
           SET multi_tenant = ?
      |] (Traction.Only $ MultiTenant == settings)
