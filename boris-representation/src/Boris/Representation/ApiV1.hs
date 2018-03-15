{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Representation.ApiV1 (
    GetCommit (..)
  , GetQueue (..)
  , GetBuilds (..)
  , GetBuild (..)
  , PostBuildRequest (..)
  , PutBuildIgnore (..)
  , GetProjects (..)
  , GetProject (..)
  , GetScoreboard (..)
  , PostDiscover (..)
  , PostHeartbeatResponse (..)
  , PostAcknowledgeRequest (..)
  , PostAcknowledgeResponse (..)
  , PostAvowRequest (..)
  , PostAvowResponse (..)
  , PostCompleteRequest (..)
  , PostCompleteResponse (..)
  , GetLogs (..)
  ) where

import           Boris.Core.Data

import           Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.=))

import           P

data GetCommit =
  GetCommit Project [BuildId]
  deriving (Eq, Show)

instance ToJSON GetCommit where
  toJSON (GetCommit p bs) =
    object [
        "project" .= renderProject p
      , "builds" .= (fmap renderBuildId . sortBuildIds) bs
      ]

instance FromJSON GetCommit where
  parseJSON =
    withObject "GetCommit" $ \o ->
      GetCommit
        <$> (fmap Project $ o .: "project")
        <*> (o .: "builds" >>= pure . fmap BuildId)

newtype GetQueue =
  GetQueue {
      getQueue :: QueueSize
    } deriving (Eq, Ord, Show)

instance ToJSON GetQueue where
  toJSON (GetQueue q) =
    object ["size" .= getQueueSize q]

instance FromJSON GetQueue where
  parseJSON =
    withObject "GetQueue" $ \o ->
      (GetQueue . QueueSize) <$> o .: "size"

newtype PostBuildRequest =
  PostBuildRequest {
      getPostBuildsRef :: Maybe Ref
    } deriving (Eq, Ord, Show)

instance FromJSON PostBuildRequest where
  parseJSON =
    withObject "PostBuilds" $ \o ->
                                PostBuildRequest
        <$> (fmap . fmap) Ref (o .:? "ref")

instance ToJSON PostBuildRequest where
  toJSON (PostBuildRequest r) =
    object [
        "ref" .= fmap renderRef r
      ]






data GetBuilds =
    GetBuilds {
        getBuilds :: BuildTree
      } deriving (Eq, Ord, Show)

instance FromJSON GetBuilds where
  parseJSON =
    withObject "GetBuilds" $ \o ->
      fmap GetBuilds $
        BuildTree
          <$> (fmap Project $ o .: "project")
          <*> (fmap Build $ o .: "build")
          <*> (o .: "details" >>= mapM (withObject "BuildTreeRef" $ \oo ->
                  BuildTreeRef
                    <$> fmap Ref (oo .: "ref")
                    <*> (fmap . fmap) BuildId (oo .: "build_ids")
                   ))

instance ToJSON GetBuilds where
  toJSON (GetBuilds (BuildTree p b ds)) =
    object [
        "project" .= renderProject p
      , "build" .= renderBuild b
      , "details" .= with ds (\(BuildTreeRef ref ids) -> object [
            "ref" .= renderRef ref
          , "build_ids" .= fmap renderBuildId (sortBuildIds ids)
          ])
      ]

newtype GetBuild =
  GetBuild {
      getBuild :: BuildData
    } deriving (Eq, Ord, Show)

newtype PostHeartbeatResponse =
  PostHeartbeatResponse {
      postHeartbeatCancelled :: BuildCancelled
    } deriving (Eq, Ord, Show)

instance FromJSON PostHeartbeatResponse where
  parseJSON =
    withObject "PostHeartbeatResponse" $ \o ->
      fmap PostHeartbeatResponse $
        bool BuildNotCancelled BuildCancelled <$> (o .: "build-cancelled")

instance ToJSON PostHeartbeatResponse where
  toJSON (PostHeartbeatResponse cancelled) =
    object [
        "build-cancelled" .= case cancelled of BuildCancelled -> True; BuildNotCancelled -> False
      ]


data PostAvowRequest =
  PostAvowRequest {
      postAvowRequestRef :: Ref
    , postAvowRequestCommit :: Commit
    } deriving (Eq, Ord, Show)

instance FromJSON PostAvowRequest where
  parseJSON =
    withObject "PostAvowRequest" $ \o ->
      PostAvowRequest
        <$> (fmap Ref $ o .: "ref")
        <*> (fmap Commit $ o .: "commit")

instance ToJSON PostAvowRequest where
  toJSON (PostAvowRequest ref commit) =
    object [
        "ref" .= renderRef ref
      , "commit" .= renderCommit commit
      ]

data PostAvowResponse =
    PostAvowResponse
    deriving (Eq, Ord, Show)

instance FromJSON PostAvowResponse where
  parseJSON =
    withObject "PostAvowResponse" $ \_ ->
      pure PostAvowResponse

instance ToJSON PostAvowResponse where
  toJSON PostAvowResponse =
    object [
      ]

data PostDisavowResponse =
    PostDisavowResponse
    deriving (Eq, Ord, Show)

instance FromJSON PostDisavowResponse where
  parseJSON =
    withObject "PostDisavowResponse" $ \_ ->
      pure PostDisavowResponse

instance ToJSON PostDisavowResponse where
  toJSON PostDisavowResponse =
    object [
      ]

data PostAcknowledgeRequest =
    PostAcknowledgeRequest
    deriving (Eq, Ord, Show)

instance FromJSON PostAcknowledgeRequest where
  parseJSON =
    withObject "PostAcknowledgeRequest" $ \_ ->
      pure PostAcknowledgeRequest

instance ToJSON PostAcknowledgeRequest where
  toJSON PostAcknowledgeRequest =
    object [
      ]

newtype PostAcknowledgeResponse =
  PostAcknowledgeResponse {
      postAcknowledge :: Acknowledge
    } deriving (Eq, Ord, Show)

instance FromJSON PostAcknowledgeResponse where
  parseJSON =
    withObject "PostAcknowledgeResponse" $ \o ->
      fmap PostAcknowledgeResponse $
        bool AlreadyRunning Accept <$> (o .: "accept")

instance ToJSON PostAcknowledgeResponse where
  toJSON (PostAcknowledgeResponse acknowledge) =
    object [
        "accept" .= case acknowledge of Accept -> True; AlreadyRunning -> False
      ]

instance FromJSON GetBuild where
  parseJSON =
    withObject "GetBuild" $ \o ->
      fmap GetBuild $
        BuildData
          <$> (fmap BuildId $ o .: "build_id")
          <*> (fmap Project $ o .: "project")
          <*> (fmap Build $ o .: "build")
          <*> ((fmap . fmap) Ref $ o .:? "ref")
          <*> ((fmap . fmap) Commit $ o .:? "commit")
          <*> (o .:? "queued")
          <*> (o .:? "started")
          <*> (o .:? "completed")
          <*> (o .:? "heartbeat")
          <*> ((fmap . fmap) (bool BuildKo BuildOk) $ o .:? "result")
          <*> ((fmap . fmap) (bool BuildNotCancelled BuildCancelled) $ o .:? "cancelled")

instance ToJSON GetBuild where
  toJSON (GetBuild b) =
    object [
        "build_id" .= (renderBuildId . buildDataId) b
      , "project" .= (renderProject . buildDataProject) b
      , "build" .= (renderBuild . buildDataBuild) b
      , "ref" .= (fmap renderRef . buildDataRef) b
      , "commit" .= (fmap renderCommit . buildDataCommit) b
      , "queued" .= buildDataQueueTime b
      , "started" .= buildDataStartTime b
      , "completed" .= buildDataEndTime b
      , "heartbeat" .= buildDataHeartbeatTime b
      , "result" .= (flip fmap (buildDataResult b) $ \bb -> case bb of BuildOk -> True; BuildKo -> False)
      , "cancelled" .= (flip fmap (buildDataCancelled b) $ \bb -> case bb of BuildCancelled -> True; BuildNotCancelled -> False)
      ]

newtype PutBuildIgnore =
  PutBuildIgnore {
      getPutBuildIgnore :: Bool
    } deriving (Eq, Ord, Show)

instance ToJSON PutBuildIgnore where
  toJSON (PutBuildIgnore i) =
    object [
        "ignore" .= i
      ]

instance FromJSON PutBuildIgnore where
  parseJSON =
    withObject "PutBuildIgnore" $ \o ->
      PutBuildIgnore
        <$> o .: "ignore"

newtype GetProjects =
    GetProjects { getProjects :: [Project] }
    deriving (Eq, Ord, Show)

instance ToJSON GetProjects where
  toJSON (GetProjects ps) =
    object [
        "projects" .= fmap renderProject ps
      ]

instance FromJSON GetProjects where
  parseJSON=
    withObject "GetProjects" $ \o ->
      fmap GetProjects $
        o .: "projects" >>= pure . fmap Project

data GetProject =
    GetProject {
        getProjectName :: Project
      , getProjectBuilds :: [Build]
      } deriving (Eq, Ord, Show)

instance ToJSON GetProject where
  toJSON (GetProject p bs) =
    object [
        "project" .= renderProject p
      , "builds" .= fmap renderBuild bs
      ]

instance FromJSON GetProject where
  parseJSON =
    withObject "GetProject" $ \o ->
      GetProject
        <$> (fmap Project $ o .: "project")
        <*> (o .: "builds" >>= pure . fmap Build)

data GetScoreboard =
    GetScoreboard [Result]
    deriving (Eq, Ord, Show)

instance ToJSON GetScoreboard where
  toJSON (GetScoreboard rs) =
    object [
        "builds" .= fmap fromResult rs
      ]

instance FromJSON GetScoreboard where
  parseJSON =
    withObject "GetScoreboard" $ \o ->
      GetScoreboard
        <$> (o .: "builds" >>= mapM (withObject "Result" $ \oo ->
            Result
              <$> (fmap BuildId $ oo .: "build_id")
              <*> (fmap Project $ oo .: "project")
              <*> (fmap Build $ oo .: "build")
              <*> ((fmap . fmap) Ref $ oo .: "ref")
              <*> (oo .: "result" >>= fromMaybeM (fail "Invalid build result") . parseBuildResult)
          ))

fromResult :: Result -> Value
fromResult r =
  object [
      "build_id" .= (renderBuildId . resultBuildId) r
    , "project" .= (renderProject . resultProject) r
    , "build" .= (renderBuild . resultBuild) r
    , "ref" .= (fmap renderRef . resultRef) r
    , "result" .= (renderBuildResult . resultBuildResult) r
    ]




data PostDiscover =
  PostDiscover {
      postDiscoverProject :: Project
    , postDiscoverGuts :: [DiscoverInstance]
    } deriving (Eq, Ord, Show)

instance FromJSON PostDiscover where
  parseJSON =
    withObject "PostDiscover" $ \o ->
      PostDiscover
        <$> (Project <$> o .: "project")
        <*> (o .: "discover" >>= mapM (withObject "DiscoverInstance" $ \oo ->
          DiscoverInstance
            <$> (Build <$> oo .: "build")
            <*> (Ref <$> oo .: "ref")
            <*> (Commit <$> oo .: "commit")))

instance ToJSON PostDiscover where
  toJSON x =
    object [
        "project" .= (renderProject . postDiscoverProject $ x)
      , "discover" .= (with (postDiscoverGuts x) $ \(DiscoverInstance build ref commit) -> object [
          "build" .= renderBuild build
        , "ref" .= renderRef ref
        , "commit" .= renderCommit commit
        ])
      ]

data PostCompleteRequest =
    PostCompleteRequest {
      postCompleteRequestResult :: BuildResult
    } deriving (Eq, Ord, Show)

instance FromJSON PostCompleteRequest where
  parseJSON =
    withObject "PostCompleteRequest" $ \o ->
      PostCompleteRequest
        <$> (fmap (bool BuildKo BuildOk) $ o .: "result")

instance ToJSON PostCompleteRequest where
  toJSON (PostCompleteRequest r) =
    object [
        "result" .= case r of BuildOk -> True; BuildKo -> False
      ]

data PostCompleteResponse =
    PostCompleteResponse
    deriving (Eq, Ord, Show)

instance FromJSON PostCompleteResponse where
  parseJSON =
    withObject "PostCompleteResponse" $ \_ ->
      pure PostCompleteResponse

instance ToJSON PostCompleteResponse where
  toJSON PostCompleteResponse =
    object [
      ]

newtype GetLogs =
  GetLogs {
      getLogs :: LogData
    } deriving (Eq, Ord, Show)

instance ToJSON GetLogs where
  toJSON (GetLogs (DBLog ls)) =
    object [
      "result" .= renderDBLogs ls
    ]

instance FromJSON GetLogs where
  parseJSON =
    -- FIXME
    withObject "GetLogs" $ \_ ->
      pure . GetLogs $ DBLog []
