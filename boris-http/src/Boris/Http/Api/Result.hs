{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Result (
    scoreboard
  , status
  ) where


import           Boris.Core.Data.Build
import qualified Boris.Http.Db.Query as Query

import           P

import           Traction.Control (MonadDb)


scoreboard :: MonadDb m => m [Result]
scoreboard =
  Query.results

status :: MonadDb m => m [Result]
status =
  filter ((==) BuildKo . resultBuildResult)
    <$> scoreboard
