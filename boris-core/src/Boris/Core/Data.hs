{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Core.Data (
    BuildResult (..)
  , Acknowledge (..)
  , BuildCancelled (..)
  , BuildData (..)
  , Result (..)
  , BuildTree (..)
  , BuildTreeRef (..)
  , QueueSize (..)
  , renderBuildResult
  , parseBuildResult
  ) where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Project
import           Boris.Core.Data.Repository

import qualified Data.Text as T
import           Data.Time (UTCTime)

import           P

data BuildResult =
    BuildOk
  | BuildKo
    deriving (Eq, Show, Ord, Enum, Bounded)

renderBuildResult :: BuildResult -> Text
renderBuildResult r =
  case r of
    BuildOk ->
      "ok"
    BuildKo ->
      "ko"

parseBuildResult :: Text -> Maybe BuildResult
parseBuildResult r =
  case r of
    "ok" ->
      Just BuildOk
    "ko" ->
      Just BuildKo
    _ ->
      Nothing

data Acknowledge =
    Accept
  | AlreadyRunning
    deriving (Eq, Ord, Show, Enum, Bounded)

data BuildCancelled =
    BuildCancelled
  | BuildNotCancelled
    deriving (Eq, Ord, Show, Enum, Bounded)

data BuildData =
  BuildData {
      buildDataId :: BuildId
    , buildDataProject :: Project
    , buildDataBuild :: Build
    , buildDataRef :: Maybe Ref
    , buildDataCommit :: Maybe Commit
    , buildDataQueueTime :: Maybe UTCTime
    , buildDataStartTime :: Maybe UTCTime
    , buildDataEndTime :: Maybe UTCTime
    , buildDataHeartbeatTime :: Maybe UTCTime
    , buildDataResult :: Maybe BuildResult
    , buildDataCancelled :: Maybe BuildCancelled
    } deriving (Eq, Ord, Show)

data Result =
  Result {
      resultBuildId :: !BuildId
    , resultProject :: !Project
    , resultBuild :: !Build
    , resultRef :: !(Maybe Ref)
    , resultBuildResult :: !BuildResult
    } deriving (Eq, Show, Ord)


data BuildTree =
  BuildTree {
      buildTreeProject :: Project
    , buildTreeBuild :: Build
    , buildTreeRefs :: [BuildTreeRef]
    } deriving (Eq, Ord, Show)

data BuildTreeRef =
  BuildTreeRef {
      buildTreeRef :: Ref
    , buildTreeIds :: [BuildId]
    } deriving (Eq, Ord, Show)

newtype QueueSize =
  QueueSize {
      getQueueSize :: Int
    } deriving (Eq, Ord, Show)
