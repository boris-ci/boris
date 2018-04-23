{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Boris.Core.Data (
    Environment (..)
  , Source (..)
  , sourceFromInt
  , sourceToInt
  , Owner (..)
  , OwnerId (..)
  , OwnerName (..)
  , OwnerType (..)
  , ownerTypeFromInt
  , ownerTypeToInt
  , Definition (..)
  , ProjectId (..)
  , Project (..)
  , Build (..)
  , BuildId (..)
  , Repository (..)
  , LocalRepository (..)
  , Commit (..)
  , Ref (..)
  , Pattern (..)
  , Registration (..)
  , BuildResult (..)
  , Acknowledge (..)
  , WorkspacePath (..)
  , Workspace (..)
  , BuildCancelled (..)
  , BuildData (..)
  , Result (..)
  , BuildTree (..)
  , BuildTreeRef (..)
  , QueueSize (..)
  , Settings (..)
  , renderBuildResult
  , parseBuildResult
  , renderRegistration
  , parseRegistration
  , pathOf
  , pathOfMirror
  , pathOfWorkingCopy
  , repositoryOfMirror
  , repositoryOfWorkingCopy
  , sortBuildIds
  , newBuild
  ) where

import qualified Data.List as L
import qualified Data.Text as T
import           Data.Time (UTCTime, formatTime, defaultTimeLocale)

import qualified Data.Map.Strict as M

import           P

import           System.FilePath (FilePath, (</>))
import qualified "Glob" System.FilePath.Glob as G

newtype Environment =
  Environment {
      renderEnvironment :: Text
    } deriving (Eq, Show)

data Source =
    GitHubSource
  | BorisSource
    deriving (Eq, Ord, Show, Enum, Bounded)

sourceToInt :: Source -> Int64
sourceToInt s =
  case s of
    GitHubSource ->
      0
    BorisSource ->
      1

sourceFromInt :: Int64 -> Maybe Source
sourceFromInt n =
  case n of
    0 ->
      Just GitHubSource
    1 ->
      Just BorisSource
    _ ->
      Nothing

newtype OwnerId =
  OwnerId {
      getOwnerId :: Int64
    } deriving (Eq, Ord, Show)

newtype OwnerName =
  OwnerName {
      getOwnerName :: Int64
    } deriving (Eq, Ord, Show)

-- FIX this isn't right anymore, need to update to reflect sketch
data OwnerType =
    GitHubOwner
  | BorisUser
  | BorisSystem
    deriving (Eq, Ord, Show, Enum, Bounded)

ownerTypeToInt :: OwnerType -> Int64
ownerTypeToInt o =
  case o of
    GitHubOwner ->
      0
    BorisUser ->
      1
    BorisSystem ->
      2

ownerTypeFromInt :: Int64 -> Maybe OwnerType
ownerTypeFromInt n =
  case n of
    0 ->
      Just GitHubOwner
    1 ->
      Just BorisUser
    2 ->
      Just BorisSystem
    _ ->
      Nothing

data Owner =
  Owner {
      ownerId :: OwnerId
    , ownerName :: OwnerName
    , ownerType :: OwnerType
    } deriving (Eq, Ord, Show)

data Definition =
  Definition {
      definitionId :: ProjectId
    , definitionSource :: Source
    , definitionOwner :: Owner
    , definitionProject :: Project
    , definitionRepository :: Repository
    } deriving (Eq, Ord, Show)

newtype Project =
  Project {
      renderProject :: Text
    } deriving (Eq, Show, Ord)

newtype ProjectId =
  ProjectId {
      getProjectId :: Int64
    } deriving (Eq, Show, Ord)

newtype Build =
  Build {
      renderBuild :: Text
    } deriving (Eq, Show, Ord)

newtype BuildId =
  BuildId {
      renderBuildId :: Text
    } deriving (Eq, Show)


instance Ord BuildId where
  compare b1 b2 =
    let
      asInt = (readMaybe :: [Char] -> Maybe Int) . T.unpack . renderBuildId
    in
      asInt b1 `compare` asInt b2

newtype Repository =
  Repository {
      renderRepository :: Text
    } deriving (Eq, Show, Ord)

newtype LocalRepository =
  LocalRepository {
      renderLocalRepository :: Text
    } deriving (Eq, Show, Ord)

newtype Ref =
  Ref {
      renderRef :: Text
    } deriving (Eq, Show, Ord)

newtype Pattern =
  Pattern {
      renderPattern :: Text
    } deriving (Eq, Show, Ord)

newtype Commit =
  Commit {
      renderCommit :: Text
    } deriving (Eq, Show, Ord)

newtype WorkspacePath =
  WorkspacePath {
      renderWorkspacePath :: Text
    } deriving (Eq, Show)

data Workspace =
  Workspace {
      workspacePath :: WorkspacePath
    , workspaceId :: BuildId
    } deriving (Eq, Show)

data Registration =
  Registration {
      registrationProject :: Project
    , registrationRepository :: Repository
    } deriving (Eq, Show)

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

renderRegistration :: Registration -> Text
renderRegistration r =
  T.intercalate "=" [renderProject . registrationProject $ r, renderRepository . registrationRepository $ r]

parseRegistration :: Text -> Maybe Registration
parseRegistration t =
  case T.splitOn "=" t of
    [p,r] ->
      Just $ Registration (Project p) (Repository r)
    _ ->
      Nothing

pathOfMirror :: Workspace -> FilePath
pathOfMirror =
  (</> "mirror") . pathOf

repositoryOfMirror :: Workspace -> LocalRepository
repositoryOfMirror =
  LocalRepository . T.pack . pathOfMirror

pathOfWorkingCopy :: Workspace -> FilePath
pathOfWorkingCopy =
  (</> "work") . pathOf

repositoryOfWorkingCopy :: Workspace -> LocalRepository
repositoryOfWorkingCopy =
  LocalRepository . T.pack . pathOfWorkingCopy

pathOf :: Workspace -> FilePath
pathOf w =
  (T.unpack . renderWorkspacePath . workspacePath $ w) </> (T.unpack . renderBuildId . workspaceId $ w)

sortBuildIds :: [BuildId] -> [BuildId]
sortBuildIds =
  L.reverse . L.sort

newBuild :: Text -> Maybe Build
newBuild b =
  emptyOrValue (T.isInfixOf "/" b) $
    Build b


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

data Settings =
    SingleTenantSettings
  | MultiTenantSettings
    deriving (Eq, Ord, Show)
