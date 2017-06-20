{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Scoreboard (
    ScoreboardError (..)
  , fetchLatestMasterBuilds
  , fetchBrokenMasterBuilds
  , renderScoreboardError
  ) where


import           Boris.Core.Data
import qualified Boris.Store.Results as SR
import qualified Boris.Store.Index as SI

import           Control.Monad.Trans.Class (lift)

import           Mismi (Error, runAWST, renderError)
import           Mismi.Amazonka (Env)

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)


data ScoreboardError =
    ScoreboardAwsError Error
  | ScoreboardResultError SR.JsonError


fetchLatestMasterBuilds :: Env -> Environment -> EitherT ScoreboardError IO [SR.Result]
fetchLatestMasterBuilds env e =
  runAWST env ScoreboardAwsError $ do
    rs <- firstT ScoreboardResultError $
      SR.compress e

    flip filterM rs $ \(SR.Result _ p b _ _) ->
      lift . fmap not $ SI.isBuildDisabled e p b


fetchBrokenMasterBuilds :: Env -> Environment -> EitherT ScoreboardError IO [SR.Result]
fetchBrokenMasterBuilds env e =
  filter ((==) BuildKo . SR.resultBuildResult)
    <$> fetchLatestMasterBuilds env e

renderScoreboardError :: ScoreboardError -> Text
renderScoreboardError se =
  case se of
    ScoreboardAwsError e ->
      renderError e
    ScoreboardResultError e ->
      "Error fetching results: " <> SR.jsonError e
