{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Core.Data.Build (
    Build (..)
  , BuildId (..)
  , Commit (..)
  , Ref (..)
  , Pattern (..)
  , sortBuildIds
  , newBuild
  , renderBuildId
  ) where

import qualified Data.List as List
import qualified Data.Text as Text

import           P

newtype Build =
  Build {
      renderBuild :: Text
    } deriving (Eq, Ord, Show)

newtype BuildId =
  BuildId {
      getBuildId :: Int64
    } deriving (Eq, Ord, Show)


renderBuildId :: BuildId -> Text
renderBuildId =
  Text.pack . show . getBuildId

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

sortBuildIds :: [BuildId] -> [BuildId]
sortBuildIds =
  List.reverse . List.sort

newBuild :: Text -> Maybe Build
newBuild b =
  emptyOrValue (Text.isInfixOf "/" b) $
    Build b
