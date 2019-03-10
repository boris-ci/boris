{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Core.Data.Discover (
    DiscoverId (..)
  , renderDiscoverId
  , Discover (..)
  ) where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Keyed
import           Boris.Core.Data.Project
import           Boris.Prelude

import qualified Data.Text as Text
import           Data.Time (UTCTime)


newtype DiscoverId =
  DiscoverId {
      getDiscoverId :: Int64
    } deriving (Eq, Ord, Show)

renderDiscoverId :: DiscoverId -> Text
renderDiscoverId =
  Text.pack . show . getDiscoverId

data Discover =
  Discover {
      discoverProject :: Keyed ProjectId Project
    , discoverCancelled :: Maybe BuildCancelled
    , discoverQueueTime :: Maybe UTCTime
    , discoverStartTime :: Maybe UTCTime
    , discoverEndTime :: Maybe UTCTime
    , discoverHeartbeatTime :: Maybe UTCTime
    } deriving (Eq, Ord, Show)
