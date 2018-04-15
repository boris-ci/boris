{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Result (
    scoreboard
  , status
  ) where


import           Boris.Core.Data
import qualified Boris.Http.Db.Query as Query

import           P

import           System.IO (IO)

import           Traction.Control (MonadDb)

import           X.Control.Monad.Trans.Either (EitherT)

scoreboard :: MonadDb m => m [Result]
scoreboard =
  Query.results

status :: MonadDb m => m [Result]
status =
  filter ((==) BuildKo . resultBuildResult)
    <$> scoreboard
