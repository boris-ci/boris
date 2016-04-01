{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Project (
    list
  , fetch
  , discover
  ) where

import           Boris.Core.Data
import           Boris.Client.Http (BorisHttpClientError (..))
import qualified Boris.Client.Http as H

import           Data.Aeson (FromJSON (..), withObject, (.:))

import           P

import           Snooze.Balance.Control (BalanceConfig)

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)

list :: BalanceConfig -> EitherT BorisHttpClientError IO [Project]
list c =
  fmap (maybe [] getProjects) $
    H.get c ["project"]

fetch :: BalanceConfig -> Project -> EitherT BorisHttpClientError IO [Build]
fetch c p =
  fmap (maybe [] getProjectBuilds) $
    H.get c ["project", renderProject p]

discover :: BalanceConfig -> Project -> EitherT BorisHttpClientError IO ()
discover c p =
  H.post_ c ["project", renderProject p]

newtype GetProjects =
  GetProjects {
      getProjects :: [Project]
    } deriving (Eq, Show)

instance FromJSON GetProjects where
  parseJSON =
    withObject "GetProjects" $ \o ->
      fmap (GetProjects . fmap Project) $ o .: "projects"

newtype GetProject =
  GetProject {
      getProjectBuilds :: [Build]
    } deriving (Eq, Show)

instance FromJSON GetProject where
  parseJSON =
    withObject "GetProject" $ \o ->
      fmap (GetProject . fmap Build) $ o .: "builds"
