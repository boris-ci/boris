{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Scoreboard (
    ScoreboardError (..)
  , fetchLatestMasterBuilds
  , fetchBrokenMasterBuilds
  , renderScoreboardError
  ) where


import           Boris.Core.Data
import           Boris.Http.Data
import           Boris.Http.Repository (ConfigError (..), list, renderConfigError)
import qualified Boris.Store.Build as SB
import qualified Boris.Store.Index as SI

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)

import           Data.Time (UTCTime, getCurrentTime, diffUTCTime)

import           Mismi (Error, runAWST, renderError)
import           Mismi.Amazonka (Env)

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, bimapEitherT)


data ScoreboardError =
    ScoreboardConfigError ConfigError
  | ScoreboardFetchError SB.FetchError
  | ScoreboardAwsError Error


fetchLatestMasterBuilds :: Env -> Environment -> ConfigLocation -> EitherT ScoreboardError IO [SB.BuildData]
fetchLatestMasterBuilds env e c = do
  projects' <- bimapEitherT ScoreboardConfigError id $ list env c
  now <- liftIO getCurrentTime
  runAWST env ScoreboardAwsError . fmap (catMaybes . mconcat) . forM projects' $ \p -> do
    builds <- lift $ SI.getProjects e p
    forM builds $ \b -> do
      -- We only care about master builds for scoreboard
      buildIds <- lift $ SI.getBuildIds e p b (Ref "refs/heads/master")
      bimapEitherT ScoreboardFetchError id
        -- Find the first build with a result
        . findMapM (fmap (find (\bd -> hasResult bd && isRecent now bd) . Just) . SB.fetch e)
        . sortBuildIds
        $ buildIds

fetchBrokenMasterBuilds :: Env -> Environment -> ConfigLocation -> EitherT ScoreboardError IO [SB.BuildData]
fetchBrokenMasterBuilds env e c =
  filter ((==) (Just BuildKo) . SB.buildDataResult)
    <$> fetchLatestMasterBuilds env e c

hasResult :: SB.BuildData -> Bool
hasResult =
  isJust . SB.buildDataResult

isRecent :: UTCTime -> SB.BuildData -> Bool
isRecent now =
  -- Two weeks
  maybe False (\d -> diffUTCTime now d < 60 * 60 * 24 * 14) . SB.buildDataEndTime

renderScoreboardError :: ScoreboardError -> Text
renderScoreboardError se =
  case se of
    ScoreboardConfigError e ->
      renderConfigError e
    ScoreboardFetchError e ->
      SB.renderFetchError e
    ScoreboardAwsError e ->
      renderError e
