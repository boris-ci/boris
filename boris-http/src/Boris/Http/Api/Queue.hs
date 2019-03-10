{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Queue (
    next
  ) where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Discover
import           Boris.Core.Data.Keyed
import qualified Boris.Http.Db.Queue as QueueDb
import           Boris.Prelude

import           Traction.Control (Db)


next :: Db (Maybe (Either (Keyed DiscoverId Discover) (Keyed BuildId Build)))
next =
  QueueDb.next
