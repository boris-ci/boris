{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Store.Error (
    StoreError (..)
  , renderStoreError

  , RegisterError (..)
  , renderRegisterError

  , FetchError (..)
  , renderFetchError
  ) where

import           Boris.Core.Data

import qualified Mismi.Control as Mismi

import           P

import           Traction.Control (DbError)
import qualified Traction.Control as Traction


data StoreError =
    DynamoBackendError Mismi.Error
  | PostgresBackendError DbError
  | InitialisationError Text
  | TickError Environment
  | ResultsError Text -- FIX
    deriving (Show)

renderStoreError :: StoreError -> Text
renderStoreError err =
  case err of
    DynamoBackendError e ->
      mconcat ["Dynamo specific backend error: ", Mismi.renderError e]
    PostgresBackendError e ->
      mconcat ["Postgres specific backend error: ", Traction.renderDbError e]
    InitialisationError e ->
      mconcat ["Illspecified initialisation error: ", e]
    TickError e ->
      mconcat ["Unable to tick build-id for environment: ", renderEnvironment e]
    ResultsError e ->
      mconcat ["Illspecified results error: ", e]

data RegisterError =
    BuildIdAlreadyRegistered Environment Project Build BuildId
  | RegisterStoreError StoreError
    deriving (Show)

data FetchError =
    MissingBuild BuildId
  | MissingProject BuildId
  | InvalidQueueTime BuildId
  | InvalidStartTime BuildId
  | InvalidEndTime BuildId
  | InvalidHeartbeatTime BuildId
  | FetchBackendError StoreError
    deriving (Show)

renderRegisterError :: RegisterError -> Text
renderRegisterError err =
  case err of
    BuildIdAlreadyRegistered e p b i ->
      mconcat [
          "Build could not be registered, already exists"
        , ": environment = " , renderEnvironment e
        , ", project = ", renderProject p
        , ", build = ", renderBuild b
        , ", build-id = ", renderBuildId i
        ]
    RegisterStoreError e ->
      mconcat ["Store error while registering build: ", renderStoreError e]

renderFetchError :: FetchError -> Text
renderFetchError err =
  case err of
    MissingBuild i ->
      mconcat ["Invalid item (missing build) on fetch of build-id: ", renderBuildId i]
    MissingProject i ->
      mconcat ["Invalid item (missing project) on fetch of build-id: ", renderBuildId i]
    InvalidQueueTime i ->
      mconcat ["Invalid item (invalid queue-time) on fetch of build-id: ", renderBuildId i]
    InvalidStartTime i ->
      mconcat ["Invalid item (invalid start-time) on fetch of build-id: ", renderBuildId i]
    InvalidEndTime i ->
      mconcat ["Invalid item (invalid end-time) on fetch of build-id: ", renderBuildId i]
    InvalidHeartbeatTime i ->
      mconcat ["Invalid item (invalid heartbeat-time) on fetch of build-id: ", renderBuildId i]
    FetchBackendError e ->
      mconcat ["Store error while fetching build: ", renderStoreError e]
