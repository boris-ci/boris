{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Representation.Build (
    GetBuilds (..)
  , GetBuildsDetail (..)
  , GetBuild (..)
  ) where

import           Boris.Core.Data
import           Boris.Store.Build (BuildData (..))
import qualified Boris.Store.Build as SB

import           Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.List as L

import           Jebediah.Data (GroupName (..), StreamName (..))

import           P

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
      , "build_ids" .= (L.reverse . L.sort . fmap renderBuildId) is
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
      , "result" .= (flip fmap (buildDataResult b) $ \bb -> case bb of BuildOk -> True; BuildKo -> False)
      , "log" .= (flip fmap (buildDataLog b) $ \l -> object ["group" .= (unGroupName . SB.logGroup) l, "stream" .= (unStreamName . SB.logStream) l])
      ]
