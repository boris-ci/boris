{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Scoreboard (
    scoreboard
  ) where

import           Boris.Client.Http (BorisHttpClientError (..))
import qualified Boris.Client.Http as H
import           Boris.Core.Data.Build
import           Boris.Core.Data.Project

import           Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import           Data.Aeson.Types (Parser, Value)
import qualified Data.Text as T

import           P

import           Snooze.Balance.Control (BalanceConfig)

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)

scoreboard :: BalanceConfig -> EitherT BorisHttpClientError IO [Result]
scoreboard c =
  fmap (maybe [] getResults) $
    H.get c ["scoreboard"]

newtype GetScoreboard =
  GetScoreboard {
      getResults :: [Result]
    } deriving (Eq, Show)

instance FromJSON GetScoreboard where
  parseJSON =
    withObject "GetScoreboard" $ \o ->
      fmap GetScoreboard $ o .: "builds" >>= mapM toResult


toResult :: Value -> Parser Result
toResult =
  withObject "Result" $ \o ->
    Result
      <$> (fmap BuildId $ o .: "build_id")
      <*> (fmap Project $ o .: "project")
      <*> (fmap Build $ o .: "build")
      <*> ((fmap . fmap) Ref $ o .:? "ref")
      <*> (o .: "result" >>= \u -> fromMaybeM (fail $ "Unknown BuildResult: " <> T.unpack u) $ parseBuildResult u)
