{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Core.Data.Project (
    ProjectName (..)
  , ProjectId (..)
  , Project (..)
  , isValidProjectName
  ) where

import qualified Data.Char as Char
import qualified Data.Text as Text

import           Boris.Core.Data.Repository
import           Boris.Prelude

data Project =
  Project {
      projectName :: ProjectName
    , projectRepository :: Repository
    } deriving (Eq, Ord, Show)

newtype ProjectName =
  ProjectName {
      renderProjectName :: Text
    } deriving (Eq, Ord, Show)

newtype ProjectId =
  ProjectId {
      getProjectId :: Int64
    } deriving (Eq, Ord, Show)


isValidProjectName :: ProjectName -> Bool
isValidProjectName name =
  and . fmap ($ renderProjectName name) $ [
      not . Text.null
    , Text.all isValidProjectNameChar
    ]

isValidProjectNameChar :: Char -> Bool
isValidProjectNameChar c =
  Char.isAlpha c || Char.isDigit c || c == '-' || c == '.' || c == '_'
