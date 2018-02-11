{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Store.Dynamo.Schema (
    schema
  , tTick
  , tBuild
  , tBuilds
  , tRefs
  , tProjectRefs
  , tProject
  , tProjectCommits
  , tResults
  , kTick
  , iContext
  , iBuildId
  , iProject
  , iBuild
  , iProjectBuild
  , iRef
  , iCommit
  , kContext
  , kProject
  , kProjectBuild
  , kBuild
  , kBuilds
  , kSeen
  , kDiscovered
  , kBuildId
  , kBuildIds
  , kBuildResult
  , kBuildIdState
  , kRef
  , kRefs
  , kDisabled
  , kCommit
  , kCommits
  , kQueued
  , kStartTime
  , kEndTime
  , kHeartbeatTime
  , kCancelled
  , kQueueTime
  , kLogGroup
  , kLogStream
  , kResults
  , vResults
  , kVal
  , kInt
  , kBool
  , kValL
  , kValSet
  , kTime
  , kMap
  , vGlobal
  , renderProjectBuild
  ) where

import           Boris.Core.Data

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import           Data.Time (UTCTime)

import qualified Network.AWS.DynamoDB as D

import           P

import           Spine.Data

table :: Environment -> Text -> TableName
table e n =
  TableName . T.intercalate "." $ ["boris", renderEnvironment e, n]

-- |
-- Build id state.
--
-- Key:
--  kContext :: String (always "global" for now)
--
-- Attributes:
--  kTick :: Int
--
tTick :: Environment -> TableName
tTick e =
  table e "tick"

-- |
-- Build instance state.
--
-- Key:
--  kBuildId :: String
--
-- Attributes:
--  kProject :: String
--  kBuild :: String
--  kRef :: String
--  kCommit :: String
--  kQueueTime :: String
--  kStartTime :: String
--  kEndTime :: String
--  kHeartbeatTime :: String
--  kCancelled :: Boolean
--  kBuildResult :: Boolean
--  kLogGroup :: String
--  kLogStream :: String
--
tBuild :: Environment -> TableName
tBuild e =
  table e "build"

-- |
-- Index mapping project/build to:
--   * refs
--   * currently queued instances
--
--
-- Key:
--  kProject :: String
--  kBuild :: String
--
-- Attributes:
--  kRefs :: [String]
--  kQueued :: [String]
--
tBuilds :: Environment -> TableName
tBuilds e =
  table e "project.build"

-- |
-- An index mapping project/build/refs to build-ids.
--
-- Key:
--  kProjectBuild :: String
--  kRef :: String
--
-- Attributes:
--  kBuildIds :: [String]
--
tRefs :: Environment -> TableName
tRefs e =
  table e "project.build.refs"

-- |
-- An index mapping project/refs to builds
--
-- Key:
--  kProject :: String
--  kRef :: String
--
-- Attributes:
--  kBuilds :: [String]
--
tProjectRefs :: Environment -> TableName
tProjectRefs e =
  table e "project.refs"

-- |
-- An index mapping projects to builds and commits.
--
-- Key:
--  kProject :: String
--
-- Attributes:
--  kBuilds :: [String]
--  kCommits :: [String]
--
tProject :: Environment -> TableName
tProject e =
  table e "project"

-- |
-- An index mapping project commits to build ids.
--
-- Key:
--  kProject :: String
--  kCommit :: String
--
-- Attributes:
--  kBuilds :: [String] -- NOTE: this is build-ids not build names...
--  kSeen :: [String] -- Build names that have been triggered against this commit
--
tProjectCommits :: Environment -> TableName
tProjectCommits e =
  table e "project.commit"

-- |
-- A compressed log of current build results.
--
-- Key:
--  kBuildId :: String
--
-- Attributes:
--  kResults :: [String]
--
tResults :: Environment -> TableName
tResults e =
  table e "results"

iContext :: ItemKey Text
iContext =
  ItemStringKey "context"

iBuildId :: ItemKey Text
iBuildId =
  ItemStringKey "build_id"

iProject :: ItemKey Text
iProject =
  ItemStringKey "project"

iBuild :: ItemKey Text
iBuild =
  ItemStringKey "build"

iProjectBuild :: ItemKey Text
iProjectBuild =
  ItemStringKey "project_build"

iRef :: ItemKey Text
iRef =
  ItemStringKey "refx" -- ref is now a reserved keyword

iCommit :: ItemKey Text
iCommit =
  ItemStringKey "commitx"

kContext :: Key Text
kContext =
  StringKey "context"

kTick :: Key Int
kTick =
  IntKey "tick"

kProject :: Key Text
kProject =
  StringKey "project"

kBuild :: Key Text
kBuild =
  StringKey "build"

kProjectBuild :: Key Text
kProjectBuild =
  StringKey "project_build"

kBuildId :: Text
kBuildId =
  "build_id"

kBuildIds :: Text
kBuildIds =
  "build_ids"

kStartTime :: Text
kStartTime =
  "start_time"

kBuildIdState :: Text
kBuildIdState =
  "build_id_ticker"

kQueueTime :: Key UTCTime
kQueueTime =
  TimeKey "queue_time"

kEndTime :: Text
kEndTime =
  "end_time"

kHeartbeatTime :: Text
kHeartbeatTime =
  "heartbeat_time"

kCancelled :: Text
kCancelled =
  "cancelled"

kBuildResult :: Key Bool
kBuildResult =
  BoolKey "build_result"

kBuilds :: Text
kBuilds =
  "builds"

kSeen :: Text
kSeen =
  "seen"

kDiscovered :: Text
kDiscovered =
  "discovered"

kRef :: Key Text
kRef =
  StringKey "refx" -- ref is now a reserved keyword

kRefs :: Text
kRefs =
  "refs"

kDisabled :: Text
kDisabled =
  "disabled"

kCommit :: Key Text
kCommit =
  StringKey "commitx"

kCommits :: Text
kCommits =
  "commits"

kQueued :: Text
kQueued =
  "queued"

kLogGroup :: Text
kLogGroup =
  "log_group"

kLogStream :: Text
kLogStream =
  "log_stream"

kResults :: Key [Text]
kResults =
  StringSetKey "results"

vResults :: (Text, D.AttributeValue)
vResults =
  toEncoding kContext "results"

kVal :: Text -> Key Text
kVal =
  StringKey . (<>) ":"

kInt :: Text -> Key Int
kInt =
  IntKey . (<>) ":"

kBool :: Text -> Key Bool
kBool =
  BoolKey . (<>) ":"

kValL :: Text -> Key [Text]
kValL =
  StringListKey . (<>) ":"

kValSet :: Text -> Key [Text]
kValSet =
  StringSetKey . (<>) ":"

kTime :: Text -> Key UTCTime
kTime =
  TimeKey . (<>) ":"

kMap :: Text -> Key (H.HashMap Text D.AttributeValue)
kMap =
  MapKey . (<>) ":"

vGlobal :: (Text, D.AttributeValue)
vGlobal =
  toEncoding kContext "global"

renderProjectBuild :: Project -> Build -> Text
renderProjectBuild p b =
  mconcat [renderProject p, ".", renderBuild b]

schema ::  Environment -> Schema
schema e =
  Schema [
      Table (tTick e) iContext Nothing (Throughput (ThroughputRange 2 20) (ThroughputRange 5 50))
    , Table (tBuild e) iBuildId Nothing (Throughput (ThroughputRange 2 200) (ThroughputRange 5 20))
    , Table (tBuilds e) iProject (Just iBuild) (Throughput (ThroughputRange 2 50) (ThroughputRange 5 20))
    , Table (tRefs e) iProjectBuild (Just iRef) (Throughput (ThroughputRange 2 50) (ThroughputRange 5 20))
    , Table (tProject e) iProject Nothing (Throughput (ThroughputRange 2 50) (ThroughputRange 5 100))
    , Table (tProjectRefs e) iProject (Just iRef) (Throughput (ThroughputRange 2 50) (ThroughputRange 5 50))
    , Table (tProjectCommits e) iProject (Just iCommit) (Throughput (ThroughputRange 2 50) (ThroughputRange 5 10))
    , Table (tResults e) iContext Nothing (Throughput (ThroughputRange 2 50) (ThroughputRange 5 50))
    ]
