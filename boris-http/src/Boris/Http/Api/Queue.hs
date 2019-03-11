{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Queue (
    next
  , acknowledge
  ) where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Discover
import           Boris.Core.Data.Keyed
import           Boris.Core.Data.Run
import qualified Boris.Http.Db.Queue as QueueDb
import qualified Boris.Http.Db.Run as RunDb
import           Boris.Prelude

import           Traction.Control (Db)


next :: Db (Maybe (Either (Keyed DiscoverId Discover) (Keyed BuildId Build)))
next =
  QueueDb.next


acknowledge :: RunId -> Db Acknowledge
acknowledge run = do
  ack <- QueueDb.acknowledge run
  when (ack == Accept) $
    RunDb.setStartTime run
  pure ack
