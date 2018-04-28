{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
module Boris.Http.Db.Tick (
    tick
  ) where


import           Boris.Core.Data.Build

import           P

import           Traction.Control (MonadDb)
import           Traction.Sql (sql)
import qualified Traction.Sql as Traction


tick :: MonadDb m => m BuildId
tick =
  fmap BuildId . Traction.value $ Traction.mandatory_ [sql|
      SELECT nextval('tick')
    |]
