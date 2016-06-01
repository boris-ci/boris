{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Scoreboard (
    scoreboard
  ) where

import           Boris.Client.Build
import           Boris.Client.Http (BorisHttpClientError (..))
import qualified Boris.Client.Http as H
import           Boris.Store.Build (BuildData (..))

import           Data.Aeson (FromJSON (..), withObject, (.:))

import           P

import           Snooze.Balance.Control (BalanceConfig)

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)

scoreboard :: BalanceConfig -> EitherT BorisHttpClientError IO [BuildData]
scoreboard c =
  fmap (maybe [] getBuilds) $
    H.get c ["scoreboard"]

newtype GetScoreboard =
  GetScoreboard {
      getBuilds :: [BuildData]
    } deriving (Eq, Show)

instance FromJSON GetScoreboard where
  parseJSON =
    withObject "GetScoreboard" $ \o ->
      GetScoreboard . fmap getBuild <$> o .: "builds"
