{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Build (
    trigger
  , fetch
  ) where

import           Boris.Core.Data
import           Boris.Store.Build (BuildData (..), LogData (..))
import           Boris.Client.Http (BorisHttpClientError (..))
import qualified Boris.Client.Http as H

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?))

import           Jebediah.Data (GroupName (..), StreamName (..))

import           P

import           Snooze.Balance.Control (BalanceConfig)

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)

trigger :: BalanceConfig -> Project -> Build -> EitherT BorisHttpClientError IO BuildData
trigger c p b =
  fmap getBuild $
    H.post c ["project", renderProject p , "build", renderBuild b] PostBuildRequest

fetch :: BalanceConfig -> BuildId -> EitherT BorisHttpClientError IO (Maybe BuildData)
fetch c i =
  (fmap . fmap) getBuild $
    H.get c ["build", renderBuildId i]

data PostBuildRequest =
  PostBuildRequest

instance ToJSON PostBuildRequest where
  toJSON _ =
    object []

newtype GetBuild =
  GetBuild {
      getBuild :: BuildData
    } deriving (Eq, Show)

instance FromJSON GetBuild where
  parseJSON =
    withObject "GetBuild" $ \o ->
      fmap GetBuild $
        BuildData
          <$> (fmap BuildId $ o .: "build_id")
          <*> (fmap Project $ o .: "project")
          <*> (fmap Build $ o .: "build")
          <*> ((fmap . fmap) Ref $ o .:? "ref")
          <*> (o .:? "queued")
          <*> (o .:? "started")
          <*> (o .:? "completed")
          <*> ((fmap . fmap) (bool BuildKo BuildOk) $ o .:? "result")
          <*> (do ll <- o .:? "log"
                  forM ll $ \l ->
                    flip (withObject "LogData") l $ \ld ->
                      LogData <$> (fmap GroupName $ ld .: "group") <*> (fmap StreamName $ ld .: "stream"))
