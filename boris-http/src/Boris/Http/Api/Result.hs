{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Result (
    scoreboard
  , status
  ) where


import           Boris.Core.Data.Build
import qualified Boris.Http.Db.Build as BuildDb

import           P

import           Traction.Control (MonadDb)


scoreboard :: MonadDb m => m [Result]
scoreboard =
  BuildDb.results

status :: MonadDb m => m [Result]
status =
  filter ((==) BuildKo . resultBuildResult)
    <$> scoreboard
