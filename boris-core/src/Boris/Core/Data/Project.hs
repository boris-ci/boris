{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Core.Data.Project (
    Source (..)
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
  ) where

import           Boris.Core.Data.Repository

import           P


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
