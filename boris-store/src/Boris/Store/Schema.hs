{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Store.Schema (
    schema
  , tTick
  , tBuild
  , tBuilds
  , tRefs
  , tProjectRefs
  , tProject
  , tProjectCommits
  , kTick
  , kContext
  , kProject
  , kProjectBuild
  , kBuild
  , kBuilds
  , kBuildId
  , kBuildIds
  , kBuildResult
  , kBuildIdState
  , kRef
  , kRefs
  , kCommit
  , kCommits
  , kQueued
  , kStartTime
  , kEndTime
  , kQueueTime
  , kLogGroup
  , kLogStream
  , kVal
  , vGlobal
  , vProject
  , vProjectBuild
  , vBuild
  , vBuildId
  , vBuildResult
  , vRef
  , vCommit
  , vRefOf
  , vCommitOf
  , vLogGroup
  , vLogStream
  , vTime
  , vInt
  , vStrings
  ) where

import           Boris.Core.Data

import           Control.Lens ((&), (.~))

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime, formatTime)

import           Jebediah.Data (GroupName (..), StreamName (..))

import qualified Network.AWS.DynamoDB as D

import           P

import           System.Locale (defaultTimeLocale)


table :: Environment -> Text -> Text
table e n =
  T.intercalate "." $ ["boris", renderEnvironment e, n]

-- |
-- Build id state.
--
-- Key:
--  kContext :: String (always "global" for now)
--
-- Attributes:
--  kTick :: Int
--
tTick :: Environment -> Text
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
--  kBuildResult :: Boolean
--  kLogGroup :: String
--  kLogStream :: String
--
tBuild :: Environment -> Text
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
tBuilds :: Environment -> Text
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
tRefs :: Environment -> Text
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
tProjectRefs :: Environment -> Text
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
tProject :: Environment -> Text
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
--  kBuilds :: [String]
--
tProjectCommits :: Environment -> Text
tProjectCommits e =
  table e "project.commit"

kContext :: Text
kContext =
  "context"

kTick :: Text
kTick =
  "tick"

kProject :: Text
kProject =
  "project"

kBuild :: Text
kBuild =
  "build"

kProjectBuild :: Text
kProjectBuild =
  "project_build"

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

kQueueTime :: Text
kQueueTime =
  "queue_time"

kEndTime :: Text
kEndTime =
  "end_time"

kBuildResult :: Text
kBuildResult =
  "build_result"

kBuilds :: Text
kBuilds =
  "builds"

kRef :: Text
kRef =
  "refx" -- ref is now a reserved keyword

kRefs :: Text
kRefs =
  "refs"

kCommit :: Text
kCommit =
  "commitx"

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

kVal :: Text -> Text
kVal =
  (<>) ":"

vGlobal :: (Text, D.AttributeValue)
vGlobal =
  (kContext, D.attributeValue & D.avS .~ Just "global")

vProject :: Project -> (Text, D.AttributeValue)
vProject p =
  (kProject, D.attributeValue & D.avS .~ Just (renderProject p))

vProjectBuild :: Project -> Build -> (Text, D.AttributeValue)
vProjectBuild p b =
  (kProjectBuild, D.attributeValue & D.avS .~ Just (mconcat [renderProject p, ".", renderBuild b]))

vBuild :: Build -> (Text, D.AttributeValue)
vBuild b =
  (kBuild, D.attributeValue & D.avS .~ Just (renderBuild b))

vBuildId :: BuildId -> (Text, D.AttributeValue)
vBuildId i =
  (kBuildId, D.attributeValue & D.avS .~ Just (renderBuildId i))

vBuildResult :: Text -> BuildResult -> (Text, D.AttributeValue)
vBuildResult k v =
  (k, D.attributeValue & D.avBOOL .~ Just (case v of BuildOk -> True; BuildKo -> False))

vRef :: Ref -> (Text, D.AttributeValue)
vRef =
  vRefOf kRef

vCommit :: Commit -> (Text, D.AttributeValue)
vCommit =
  vCommitOf kCommit

vRefOf :: Text -> Ref -> (Text, D.AttributeValue)
vRefOf k r =
  (k, D.attributeValue & D.avS .~ Just (renderRef r))

vCommitOf :: Text -> Commit -> (Text, D.AttributeValue)
vCommitOf k c =
  (k, D.attributeValue & D.avS .~ Just (renderCommit c))

vLogGroup :: Text -> GroupName -> (Text, D.AttributeValue)
vLogGroup k r =
  (k, D.attributeValue & D.avS .~ Just (unGroupName r))

vLogStream :: Text -> StreamName -> (Text, D.AttributeValue)
vLogStream k r =
  (k, D.attributeValue & D.avS .~ Just (unStreamName r))

vTime :: Text -> UTCTime -> (Text, D.AttributeValue)
vTime k v =
  (k, D.attributeValue & D.avS .~ Just (T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" v))

vInt :: Text -> Int -> (Text, D.AttributeValue)
vInt k v =
  (k, D.attributeValue & D.avN .~ Just (T.pack . show $ v))

vStrings :: Text -> [Text] -> (Text, D.AttributeValue)
vStrings k v =
  (k, D.attributeValue & D.avSS .~ v)

schema ::  Environment -> [D.CreateTable]
schema e = [
    D.createTable (tTick e) (D.keySchemaElement kContext D.Hash :| []) (D.provisionedThroughput 2 5)
      & D.ctAttributeDefinitions .~ [
          D.attributeDefinition kContext D.S
        ]
  , D.createTable (tBuild e) (D.keySchemaElement kBuildId D.Hash :| []) (D.provisionedThroughput 2 5)
      & D.ctAttributeDefinitions .~ [
          D.attributeDefinition kBuildId D.S
        ]
  , D.createTable (tBuilds e) (D.keySchemaElement kProject D.Hash :| [D.keySchemaElement kBuild D.Range]) (D.provisionedThroughput 2 5)
      & D.ctAttributeDefinitions .~ [
          D.attributeDefinition kProject D.S
        , D.attributeDefinition kBuild D.S
        ]
  , D.createTable (tRefs e) (D.keySchemaElement kProjectBuild D.Hash :| [D.keySchemaElement kRef D.Range]) (D.provisionedThroughput 2 5)
      & D.ctAttributeDefinitions .~ [
          D.attributeDefinition kProjectBuild D.S
        , D.attributeDefinition kRef D.S
        ]
  , D.createTable (tProject e) (D.keySchemaElement kProject D.Hash :| []) (D.provisionedThroughput 2 5)
      & D.ctAttributeDefinitions .~ [
          D.attributeDefinition kProject D.S
        ]
  , D.createTable (tProjectRefs e) (D.keySchemaElement kProject D.Hash :| [D.keySchemaElement kRef D.Range]) (D.provisionedThroughput 2 5)
      & D.ctAttributeDefinitions .~ [
          D.attributeDefinition kProject D.S
        , D.attributeDefinition kRef D.S
        ]
  , D.createTable (tProjectCommits e) (D.keySchemaElement kProject D.Hash :| [D.keySchemaElement kCommit D.Range]) (D.provisionedThroughput 2 5)
      & D.ctAttributeDefinitions .~ [
          D.attributeDefinition kProject D.S
        , D.attributeDefinition kCommit D.S
        ]
  ]
