{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Core.Data.Workspace (
    WorkspacePath (..)
  , Workspace (..)
  , pathOf
  , pathOfMirror
  , pathOfWorkingCopy
  , repositoryOfMirror
  , repositoryOfWorkingCopy
  ) where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Repository

import qualified Data.Text as Text

import           P

import           System.FilePath (FilePath, (</>))


newtype WorkspacePath =
  WorkspacePath {
      renderWorkspacePath :: Text
    } deriving (Eq, Show)

data Workspace =
  Workspace {
      workspacePath :: WorkspacePath
    , workspaceId :: BuildId
    } deriving (Eq, Show)

pathOfMirror :: Workspace -> FilePath
pathOfMirror =
  (</> "mirror") . pathOf

repositoryOfMirror :: Workspace -> LocalRepository
repositoryOfMirror =
  LocalRepository . Text.pack . pathOfMirror

pathOfWorkingCopy :: Workspace -> FilePath
pathOfWorkingCopy =
  (</> "work") . pathOf

repositoryOfWorkingCopy :: Workspace -> LocalRepository
repositoryOfWorkingCopy =
  LocalRepository . Text.pack . pathOfWorkingCopy

pathOf :: Workspace -> FilePath
pathOf w =
  (Text.unpack . renderWorkspacePath . workspacePath $ w) </> (Text.unpack . renderBuildId . workspaceId $ w)
