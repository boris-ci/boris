{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Store.Schema (
    schema
  , tBuild
  , tMeta
  , kProjectBuild
  , kBuildId
  , kBuildResult
  , kBuildIdState
  , kStartTime
  , kEndTime
  , kQueueTime
  , kVal
  , vProjectBuild
  , vBuildId
  , vBuildResult
  , vTime
  , vInt
  ) where

import           Boris.Core.Data

import           Control.Lens ((&), (.~))

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime, formatTime)

import qualified Network.AWS.DynamoDB as D

import           P

import           System.Locale (defaultTimeLocale)


table :: Environment -> Text -> Text
table e n =
  T.intercalate "." $ ["boris", renderEnvironment e, n]

tBuild :: Environment -> Text
tBuild e =
  table e "build"

tMeta :: Environment -> Text
tMeta e =
  table e "build.meta"

kProjectBuild :: Text
kProjectBuild =
  "project_build"

kBuildId :: Text
kBuildId =
  "build_id"

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

kVal :: Text -> Text
kVal =
  (<>) ":"

vProjectBuild :: Project -> Build -> (Text, D.AttributeValue)
vProjectBuild p b =
  (kProjectBuild, D.attributeValue & D.avS .~ Just (mconcat [renderProject p, ".", renderBuild b]))

vBuildId :: BuildId -> (Text, D.AttributeValue)
vBuildId i =
  (kBuildId, D.attributeValue & D.avS .~ Just (renderBuildId i))

vBuildResult :: Text -> BuildResult -> (Text, D.AttributeValue)
vBuildResult k v =
  (k, D.attributeValue & D.avBOOL .~ Just (case v of BuildOk -> True; BuildKo -> False))

vTime :: Text -> UTCTime -> (Text, D.AttributeValue)
vTime k v =
  (k, D.attributeValue & D.avS .~ Just (T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" v))

vInt :: Text -> Int -> (Text, D.AttributeValue)
vInt k v =
  (k, D.attributeValue & D.avN .~ Just (T.pack . show $ v))

schema ::  Environment -> [D.CreateTable]
schema e = [
    D.createTable (tBuild e) (D.keySchemaElement kProjectBuild D.Hash :| [D.keySchemaElement kBuildId D.Range]) (D.provisionedThroughput 10 50)
      & D.ctAttributeDefinitions .~ [
          D.attributeDefinition kProjectBuild D.S
        , D.attributeDefinition kBuildId D.S
        ]
  , D.createTable (tMeta e) (D.keySchemaElement kProjectBuild D.Hash :| []) (D.provisionedThroughput 10 50)
      & D.ctAttributeDefinitions .~ [
          D.attributeDefinition kProjectBuild D.S
        ]
  ]
