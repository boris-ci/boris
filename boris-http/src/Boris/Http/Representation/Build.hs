{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Representation.Build (
    GetBuild (..)
  ) where

import           Boris.Core.Data
import           Boris.Http.Data
import           Boris.Store.Build (BuildData (..))
import           Boris.Queue (BuildQueue (..), Request (..))

import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Text (Text)

import           Jebediah.Data (GroupName (..), StreamName (..))

import           P


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
