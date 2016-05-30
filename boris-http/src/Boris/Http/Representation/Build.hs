{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Representation.Build (
    GetBuilds (..)
  , GetBuildsDetail (..)
  , GetBuild (..)
  , PostBuilds (..)
  ) where

import           Boris.Core.Data
import           Boris.Store.Build (BuildData (..))
import qualified Boris.Store.Build as SB

import           Data.Aeson (FromJSON (..), ToJSON (..), object, (.=), (.:?), withObject)

import           Jebediah.Data (GroupName (..), StreamName (..))

import           P

newtype PostBuilds =
  PostBuilds {
      getPostBuildsRef :: Maybe Ref
    }

instance FromJSON PostBuilds where
  parseJSON =
    withObject "PostBuilds" $ \o ->
      PostBuilds
        <$> (fmap . fmap) Ref (o .:? "ref")

data GetBuilds =
  GetBuilds Project Build [GetBuildsDetail]

data GetBuildsDetail =
  GetBuildsDetail Ref [BuildId]

instance ToJSON GetBuilds where
  toJSON (GetBuilds p b ds) =
    object [
        "project" .= renderProject p
      , "build" .= renderBuild b
      , "details" .= ds
      ]

instance ToJSON GetBuildsDetail where
  toJSON (GetBuildsDetail r is) =
    object [
        "ref" .= renderRef r
      , "build_ids" .= fmap renderBuildId (sortBuildIds is)
      ]

newtype GetBuild =
  GetBuild BuildData

instance ToJSON GetBuild where
  toJSON (GetBuild b) =
    object [
        "build_id" .= (renderBuildId . buildDataId) b
      , "project" .= (renderProject . buildDataProject) b
      , "build" .= (renderBuild . buildDataBuild) b
      , "ref" .= (fmap renderRef . buildDataRef) b
      , "queued" .= buildDataQueueTime b
      , "started" .= buildDataStartTime b
      , "completed" .= buildDataEndTime b
      , "heartbeat" .= buildDataHeartbeatTime b
      , "result" .= (flip fmap (buildDataResult b) $ \bb -> case bb of BuildOk -> True; BuildKo -> False)
      , "log" .= (flip fmap (buildDataLog b) $ \l -> object ["group" .= (unGroupName . SB.logGroup) l, "stream" .= (unStreamName . SB.logStream) l])
      ]
