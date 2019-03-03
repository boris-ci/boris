{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Result (
    scoreboard
  , status
  ) where


import           Boris.Core.Data.Build
import qualified Boris.Http.Db.Query as Query
import           Boris.Prelude

import           Data.List (filter)


import           Traction.Control (MonadDb)


scoreboard :: MonadDb m => m [Result]
scoreboard =
  error "todo"
  -- Query.results

status :: MonadDb m => m [Result]
status =
  filter ((==) BuildKo . resultBuildResult)
    <$> scoreboard
