{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Boris.Core.Data (
    Environment (..)
  , Project (..)
  , Build (..)
  , BuildNamePattern
  , BuildId (..)
  , Repository (..)
  , LocalRepository (..)
  , Commit (..)
  , Ref (..)
  , Pattern (..)
  , Executor (..)
  , Command (..)
  , BuildPattern (..)
  , Specification (..)
  , Registration (..)
  , BuildResult (..)
  , BuildInstance (..)
  , DiscoverInstance (..)
  , Acknowledge (..)
  , WorkspacePath (..)
  , Workspace (..)
  , renderBuildResult
  , renderRegistration
  , parseRegistration
  , pathOf
  , pathOfMirror
  , pathOfWorkingCopy
  , repositoryOfMirror
  , repositoryOfWorkingCopy
  , sortBuildIds
  , newBuild
  , renderBuildNamePattern
  , parseBuildNamePattern
  , matchesBuild
  ) where

import qualified Data.List as L
import qualified Data.Text as T

import qualified Data.Map.Strict as M

import           P

import           System.FilePath (FilePath, (</>))
import qualified System.FilePath.Glob as G

newtype Environment =
  Environment {
      renderEnvironment :: Text
    } deriving (Eq, Show)

newtype Project =
  Project {
      renderProject :: Text
    } deriving (Eq, Show, Ord)

newtype Build =
  Build {
      renderBuild :: Text
    } deriving (Eq, Show, Ord)

newtype BuildNamePattern =
  BuildNamePattern G.Pattern
    deriving (Eq, Show)

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

newtype Executor =
  Executor {
      projects :: M.Map Project Repository
    } deriving (Eq, Show, Ord)

data Command =
  Command {
      commandName :: Text
    , commandArgs :: [Text]
    } deriving (Eq, Show)

data BuildPattern =
  BuildPattern {
      buildNamePattern :: BuildNamePattern
    , buildPattern :: Pattern
    } deriving (Eq, Show)

data BuildInstance =
  BuildInstance {
      buildSpecification :: Specification
    , buildRef :: Ref
    , buildCommit :: Commit
    } deriving (Eq, Show)

data DiscoverInstance =
  DiscoverInstance {
      discoverBuild :: Build
    , discoverRef :: Ref
    , discoverCommit :: Commit
    } deriving (Eq, Show)

data Specification =
  Specification {
      specificationBuild :: Build
    , specificationPre :: [Command]
    , specificationCommand :: [Command]
    , specificationPost :: [Command]
    , specificationSuccess :: [Command]
    , specificationFailure :: [Command]
    } deriving (Eq, Show)

data Registration =
  Registration {
      registrationProject :: Project
    , registrationRepository :: Repository
    } deriving (Eq, Show)

data BuildResult =
    BuildOk
  | BuildKo
    deriving (Eq, Show)

renderBuildResult :: BuildResult -> Text
renderBuildResult r =
  case r of
    BuildOk ->
      "ok"
    BuildKo ->
      "ko"

data Acknowledge =
    Accept
  | AlreadyRunning
    deriving (Eq, Show)

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

renderBuildNamePattern :: BuildNamePattern -> Text
renderBuildNamePattern (BuildNamePattern g) =
  T.pack . G.decompile $ g

parseBuildNamePattern :: Text -> Either Text BuildNamePattern
parseBuildNamePattern =
  let
    options =
      G.CompOptions {
          G.characterClasses = False
        , G.characterRanges = False
        , G.numberRanges = False
        , G.wildcards = True
        , G.recursiveWildcards = False
        , G.pathSepInRanges = False
        , G.errorRecovery = False
        }
  in
    bimap T.pack BuildNamePattern . G.tryCompileWith options . T.unpack

matchesBuild :: BuildNamePattern -> Build -> Bool
matchesBuild (BuildNamePattern glob) build =
  G.match
    glob
    (T.unpack $ renderBuild build)
