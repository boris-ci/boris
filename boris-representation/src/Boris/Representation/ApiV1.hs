{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Boris.Representation.ApiV1 (
    GetCommit (..)
  , GetQueue (..)
  , GetBuilds (..)
  , GetBuild (..)
  , GetDiscover (..)
  , GetNext (..)
  , PostBuildRequest (..)
  , PostDiscoverRequest (..)
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
  , PostDisavowRequest (..)
  , PostDisavowResponse (..)
  , PostCompleteRequest (..)
  , PostCompleteResponse (..)
  , GetLogs (..)
  , CreateProject (..)
  , ApiError (..)
  ) where

import           Boris.Core.Data.Agent
import           Boris.Core.Data.Build
import           Boris.Core.Data.Discover
import           Boris.Core.Data.Instance
import           Boris.Core.Data.Keyed
import           Boris.Core.Data.Log
import           Boris.Core.Data.Project
import           Boris.Core.Data.Repository
import           Boris.Prelude

import           Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.=))
import qualified Data.Text as Text


data GetCommit =
  GetCommit ProjectName [BuildId]
  deriving (Eq, Show)

instance ToJSON GetCommit where
  toJSON (GetCommit p bs) =
    object [
        "project" .= renderProjectName p
      , "builds" .= (fmap getBuildId . sortBuildIds) bs
      ]

instance FromJSON GetCommit where
  parseJSON =
    withObject "GetCommit" $ \o ->
      GetCommit
        <$> (fmap ProjectName $ o .: "project")
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

data PostBuildRequest =
  PostBuildRequest {
      getPostBuildsProject :: ProjectName
    , getPostBuildsBuild :: BuildName
    , getPostBuildsRef :: Maybe Ref
    } deriving (Eq, Ord, Show)

instance FromJSON PostBuildRequest where
  parseJSON =
    withObject "PostBuilds" $ \o ->
      PostBuildRequest
        <$> fmap ProjectName (o .: "project")
        <*> fmap BuildName (o .: "build")
        <*> (fmap . fmap) Ref (o .:? "ref")

instance ToJSON PostBuildRequest where
  toJSON (PostBuildRequest p b r) =
    object [
        "project" .= renderProjectName p
      , "build" .= renderBuildName b
      , "ref" .= fmap renderRef r
      ]

data PostDiscoverRequest =
  PostDiscoverRequest {
      getPostDiscoverProject :: ProjectName
    } deriving (Eq, Ord, Show)

instance FromJSON PostDiscoverRequest where
  parseJSON =
    withObject "PostDiscover" $ \o ->
      PostDiscoverRequest
        <$> fmap ProjectName (o .: "project")

instance ToJSON PostDiscoverRequest where
  toJSON (PostDiscoverRequest p) =
    object [
        "project" .= renderProjectName p
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
          <$> (fmap ProjectName $ o .: "project")
          <*> (fmap BuildName $ o .: "build")
          <*> (o .: "details" >>= mapM (withObject "BuildTreeRef" $ \oo ->
                  BuildTreeRef
                    <$> fmap Ref (oo .: "ref")
                    <*> (fmap . fmap) BuildId (oo .: "build_ids")
                   ))

instance ToJSON GetBuilds where
  toJSON (GetBuilds (BuildTree p b ds)) =
    object [
        "project" .= renderProjectName p
      , "build" .= renderBuildName b
      , "details" .= with ds (\(BuildTreeRef ref ids) -> object [
            "ref" .= renderRef ref
          , "build_ids" .= fmap getBuildId (sortBuildIds ids)
          ])
      ]

newtype GetBuild =
  GetBuild {
      getBuild :: Keyed BuildId Build
    } deriving (Eq, Ord, Show)


newtype GetDiscover =
  GetDiscover {
      getDiscover :: Keyed DiscoverId Discover
    } deriving (Eq, Ord, Show)

newtype GetNext =
  GetNext {
      getNext :: Either (Keyed DiscoverId Discover) (Keyed BuildId Build)
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

data PostDisavowRequest =
    PostDisavowRequest
    deriving (Eq, Ord, Show)

instance FromJSON PostDisavowRequest where
  parseJSON =
    withObject "PostDisavowRequest" $ \_ ->
      pure PostDisavowRequest

instance ToJSON PostDisavowRequest where
  toJSON PostDisavowRequest =
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

instance FromJSON GetNext where
  parseJSON =
    withObject "GetNext" $ \o ->
      (o .: "type") >>= \t ->
        case t of
          ("discover" :: Text) ->
            fmap (GetNext . Left . getDiscover) (o .: "definition")
          ("build" :: Text) ->
            fmap (GetNext . Right . getBuild) (o .: "definition")
          _ ->
            fail . mconcat $ ["Unknown queue type of: ", Text.unpack t]

instance ToJSON GetNext where
  toJSON (GetNext (Left d)) =
    object [
        "type" .= ("discover" :: Text)
      , "definition" .= toJSON (GetDiscover d)
      ]
  toJSON (GetNext (Right b)) =
    object [
        "type" .= ("build" :: Text)
      , "definition" .= toJSON (GetBuild b)
      ]

instance FromJSON GetBuild where
  parseJSON =
    withObject "GetBuild" $ \o ->
      fmap GetBuild $
        Keyed
          <$> (fmap BuildId $ o .: "build_id")
          <*> (Build
            <$> (Keyed
              <$> (fmap ProjectId $ o .: "project_id")
              <*> (Project
                <$> (fmap ProjectName $ o .: "project")
                <*> (fmap Repository $ o .: "repository")))
            <*> (fmap BuildName $ o .: "build")
            <*> ((fmap . fmap) Ref $ o .:? "ref")
            <*> ((fmap . fmap) Commit $ o .:? "commit")
            <*> ((fmap . fmap) (bool BuildKo BuildOk) $ o .:? "result")
            <*> ((fmap . fmap) (bool BuildNotCancelled BuildCancelled) $ o .:? "cancelled")
            <*> (o .:? "queued")
            <*> (o .:? "started")
            <*> (o .:? "completed")
            <*> (o .:? "heartbeat"))

instance ToJSON GetBuild where
  toJSON (GetBuild b) =
    object [
        "build_id" .= (getBuildId . keyOf) b
      , "project_id" .= (getProjectId . keyOf . buildProject . valueOf) b
      , "project" .= (renderProjectName . projectName . valueOf . buildProject . valueOf) b
      , "repository" .= (renderRepository . projectRepository . valueOf . buildProject . valueOf) b
      , "build" .= (renderBuildName . buildName . valueOf) b
      , "ref" .= (fmap renderRef . buildRef . valueOf) b
      , "commit" .= (fmap renderCommit . buildCommit . valueOf) b
      , "queued" .= (buildQueueTime . valueOf) b
      , "started" .= (buildStartTime . valueOf) b
      , "completed" .= (buildEndTime . valueOf) b
      , "heartbeat" .= (buildHeartbeatTime . valueOf) b
      , "result" .= (flip fmap ((buildResult . valueOf) b) $ \bb -> case bb of BuildOk -> True; BuildKo -> False)
      , "cancelled" .= (flip fmap ((buildCancelled . valueOf) b) $ \bb -> case bb of BuildCancelled -> True; BuildNotCancelled -> False)
      ]


instance FromJSON GetDiscover where
  parseJSON =
    withObject "GetDiscover" $ \o ->
      fmap GetDiscover $
        Keyed
          <$> (fmap DiscoverId $ o .: "discover_id")
          <*> (Discover
            <$> (Keyed
              <$> (fmap ProjectId $ o .: "project_id")
              <*> (Project
                <$> (fmap ProjectName $ o .: "project")
                <*> (fmap Repository $ o .: "repository")))
            <*> ((fmap . fmap) (bool BuildNotCancelled BuildCancelled) $ o .:? "cancelled")
            <*> (o .:? "queued")
            <*> (o .:? "started")
            <*> (o .:? "completed")
            <*> (o .:? "heartbeat"))

instance ToJSON GetDiscover where
  toJSON (GetDiscover b) =
    object [
        "discover_id" .= (getDiscoverId . keyOf) b
      , "project_id" .= (getProjectId . keyOf . discoverProject . valueOf) b
      , "project" .= (renderProjectName . projectName . valueOf . discoverProject . valueOf) b
      , "repository" .= (renderRepository . projectRepository . valueOf . discoverProject . valueOf) b
      , "queued" .= (discoverQueueTime . valueOf) b
      , "started" .= (discoverStartTime . valueOf) b
      , "completed" .= (discoverEndTime . valueOf) b
      , "heartbeat" .= (discoverHeartbeatTime . valueOf) b
      , "cancelled" .= (flip fmap ((discoverCancelled . valueOf) b) $ \bb -> case bb of BuildCancelled -> True; BuildNotCancelled -> False)
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
    GetProjects { getProjects :: [ProjectName] }
    deriving (Eq, Ord, Show)

instance ToJSON GetProjects where
  toJSON (GetProjects ps) =
    object [
        "projects" .= fmap renderProjectName ps
      ]

instance FromJSON GetProjects where
  parseJSON=
    withObject "GetProjects" $ \o ->
      fmap GetProjects $
        o .: "projects" >>= pure . fmap ProjectName

data CreateProject =
    CreateProject {
        createProjectName :: ProjectName
      , createProjectRepository :: Repository
      } deriving (Eq, Ord, Show)

instance ToJSON CreateProject where
  toJSON (CreateProject p r) =
    object [
        "project" .= renderProjectName p
      , "repository" .= renderRepository r
      ]

instance FromJSON CreateProject where
  parseJSON=
    withObject "CreateProject" $ \o ->
      CreateProject
        <$> (fmap ProjectName $ o .: "project")
        <*> (fmap Repository $ o .: "repository")

data GetProject =
    GetProject {
        getProjectName :: ProjectName
      , getProjectBuilds :: [BuildName]
      } deriving (Eq, Ord, Show)

instance ToJSON GetProject where
  toJSON (GetProject p bs) =
    object [
        "project" .= renderProjectName p
      , "builds" .= fmap renderBuildName bs
      ]

instance FromJSON GetProject where
  parseJSON =
    withObject "GetProject" $ \o ->
      GetProject
        <$> (fmap ProjectName $ o .: "project")
        <*> (o .: "builds" >>= pure . fmap BuildName)

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
              <*> (fmap ProjectName $ oo .: "project")
              <*> (fmap BuildName $ oo .: "build")
              <*> ((fmap . fmap) Ref $ oo .: "ref")
              <*> (oo .: "result" >>= fromMaybeM (fail "Invalid build result") . parseBuildResult)
          ))

fromResult :: Result -> Value
fromResult r =
  object [
      "build_id" .= (getBuildId . resultBuildId) r
    , "project" .= (renderProjectName . resultProject) r
    , "build" .= (renderBuildName . resultBuild) r
    , "ref" .= (fmap renderRef . resultRef) r
    , "result" .= (renderBuildResult . resultBuildResult) r
    ]




data PostDiscover =
  PostDiscover {
      postDiscoverGuts :: [DiscoverInstance]
    } deriving (Eq, Ord, Show)

instance FromJSON PostDiscover where
  parseJSON =
    withObject "PostDiscover" $ \o ->
      PostDiscover
        <$> (o .: "discover" >>= mapM (withObject "DiscoverInstance" $ \oo ->
          DiscoverInstance
            <$> (BuildName <$> oo .: "build")
            <*> (Ref <$> oo .: "ref")
            <*> (Commit <$> oo .: "commit")))

instance ToJSON PostDiscover where
  toJSON x =
    object [
        "discover" .= (with (postDiscoverGuts x) $ \(DiscoverInstance build ref commit) -> object [
          "build" .= renderBuildName build
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
      getLogs :: [Log]
    } deriving (Eq, Ord, Show)

instance ToJSON GetLogs where
  toJSON (GetLogs logs) =
    object [
      "logs" .= (with logs $ \log ->
           case log of
             LogEvent time chunk ->
               object [
                   "time" .= time
                 , "chunk" .= chunk
                 ]
             LogEOF ->
               object [
                   "eof" .= True
                 ]
         )
    ]

instance FromJSON GetLogs where
  parseJSON =
    withObject "GetLogs" $ \o ->
      fmap GetLogs $
        (o .: "logs") >>= mapM (withObject "Log" $ \oo ->
          o .:? "eof" >>= \x -> case x of
            Nothing ->
              LogEvent
                <$> oo .: "time"
                <*> oo .: "chunk"
            Just True ->
              pure LogEOF
            Just False ->
              fail "Invalid EOF marker.")

newtype PostLogs =
  PostLogs {
      postLogs :: [Log]
    } deriving (Eq, Ord, Show)

instance ToJSON PostLogs where
  toJSON (PostLogs logs) =
    object [
      "logs" .= (with logs $ \log ->
           case log of
             LogEvent time chunk ->
               object [
                   "time" .= time
                 , "chunk" .= chunk
                 ]
             LogEOF ->
               object [
                   "eof" .= True
                 ]
         )
    ]

instance FromJSON PostLogs where
  parseJSON =
    withObject "PostLogs" $ \o ->
      fmap PostLogs $
        (o .: "logs") >>= mapM (withObject "Log" $ \oo ->
          o .:? "eof" >>= \x -> case x of
            Nothing ->
              LogEvent
                <$> oo .: "time"
                <*> oo .: "chunk"
            Just True ->
              pure LogEOF
            Just False ->
              fail "Invalid EOF marker.")

data ApiError =
  ApiError {
      apiErrorCode :: Text
    , apiErrorMessage :: Maybe Text
    } deriving (Eq, Ord, Show)


instance ToJSON ApiError where
  toJSON (ApiError code message) =
    object [
        "code" .= code
      , "message" .= message
      ]

instance FromJSON ApiError where
  parseJSON =
    withObject "ApiError" $ \o ->
      ApiError
        <$> o .: "code"
        <*> o .:? "message"
