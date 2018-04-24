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
  ) where

import qualified Data.List as List
import qualified Data.Text as Text

import           P

newtype Build =
  Build {
      renderBuild :: Text
    } deriving (Eq, Show, Ord)

newtype BuildId =
  BuildId {
      renderBuildId :: Text -- FIX Int64
    } deriving (Eq, Show)

-- FIX
instance Ord BuildId where
  compare b1 b2 =
    let
      asInt = (readMaybe :: [Char] -> Maybe Int) . Text.unpack . renderBuildId
    in
      asInt b1 `compare` asInt b2

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
