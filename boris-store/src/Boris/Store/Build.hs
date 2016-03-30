{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Store.Build (
    RegisterError (..)
  , FetchError (..)
  , LogData (..)
  , BuildData (..)
  , renderRegisterError
  , renderFetchError
  , delete
  , register
  , acknowledge
  , complete
  , heartbeat
  , index
  , deindex
  , fetch
  ) where

import           Boris.Core.Data
import           Boris.Store.Schema
import           Boris.Store.Index

import           Control.Lens (_Just, ix, to, (&), (.~), (^?))
import           Control.Exception.Lens (handling)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Data.Time (UTCTime, getCurrentTime, parseTime)
import qualified Data.HashMap.Strict as H

import           Jebediah.Data (GroupName (..), StreamName (..))

import           Mismi (AWS)
import qualified Mismi.Amazonka as A

import qualified Network.AWS.DynamoDB as D

import           P

import           System.Locale (defaultTimeLocale)

import           X.Control.Monad.Trans.Either (EitherT, newEitherT)


data RegisterError =
    BuildIdAlreadyRegistered Environment Project Build BuildId
    deriving (Eq, Show)

data FetchError =
    MissingBuild BuildId
  | MissingProject BuildId
  | InvalidQueueTime BuildId
  | InvalidStartTime BuildId
  | InvalidEndTime BuildId
    deriving (Eq, Show)

data LogData =
  LogData {
      logGroup :: GroupName
    , logStream :: StreamName
    } deriving (Eq, Show)

data BuildData =
  BuildData {
      buildDataId :: BuildId
    , buildDataProject :: Project
    , buildDataBuild :: Build
    , buildDataRef :: Maybe Ref
    , buildDataQueueTime :: Maybe UTCTime
    , buildDataStartTime :: Maybe UTCTime
    , buildDataEndTime :: Maybe UTCTime
    , buildDataResult :: Maybe BuildResult
    , buildDataLog :: Maybe LogData
    } deriving (Eq, Show)

fetch :: Environment -> BuildId -> EitherT FetchError AWS BuildData
fetch e i = newEitherT $ do
  res <- A.send $ D.getItem (tBuild e)
    & D.giKey .~ H.fromList [
        vBuildId i
      ]
    & D.giConsistentRead .~
      Just False
  pure $ BuildData i
    <$> (maybe (Left $ MissingProject i) Right $ res ^? D.girsItem . ix kProject . D.avS . _Just . to Project)
    <*> (maybe (Left $ MissingBuild i) Right $ res ^? D.girsItem . ix kBuild . D.avS . _Just . to Build)
    <*> (Right $ res ^? D.girsItem . ix kRef . D.avS . _Just . to Ref)
    <*> (forM (res ^? D.girsItem . ix kQueueTime . D.avS . _Just) $ fromMaybeM (Left $ InvalidQueueTime i) . blat)
    <*> (forM (res ^? D.girsItem . ix kStartTime . D.avS . _Just) $ fromMaybeM (Left $ InvalidStartTime i) . blat)
    <*> (forM (res ^? D.girsItem . ix kEndTime . D.avS . _Just) $ fromMaybeM (Left $ InvalidEndTime i) . blat)
    <*> (Right . fmap (bool BuildKo BuildOk) $ res ^? D.girsItem . ix kBuildResult . D.avBOOL . _Just)
    <*> (Right $ LogData <$> res ^? D.girsItem . ix kLogGroup . D.avS . _Just . to GroupName <*> res ^? D.girsItem . ix kLogStream . D.avS . _Just . to StreamName)

blat :: Text -> Maybe UTCTime
blat =
  parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" . T.unpack

register :: Environment -> Project -> Build -> BuildId -> EitherT RegisterError AWS ()
register e p b i = do
  now <- liftIO getCurrentTime
  newEitherT $ handling D._ConditionalCheckFailedException (const . pure . Left $ BuildIdAlreadyRegistered e p b i) . fmap (const $ Right ()) $ A.send $ D.putItem (tBuild e)
    & D.piItem .~ H.fromList [
        vBuildId i
      , vTime kQueueTime now
      , vProject p
      , vBuild b
      ]
    & D.piConditionExpression .~ (Just . mconcat $ ["attribute_not_exists(", kProjectBuild, ") AND attribute_not_exists(", kBuildId, ")"])
  lift $ addQueued e p b i

index :: Environment -> Project -> Build -> BuildId -> Ref -> Commit -> AWS ()
index e p b i r c = do
  -- NOTE: We don't want to index these before acknowledge because we wouldn't of validated
  --       the project against a refs-config, which means indexing it earlier could result
  --       in rubish builds being reported that should never of been allowed through.
  liftIO . T.putStrLn $ "clear-queue"
  clearQueued e p b i
  liftIO . T.putStrLn $ "add-project"
  addProject e p b
  liftIO . T.putStrLn $ "add-project-ref"
  addProjectRef e p r b
  liftIO . T.putStrLn $ "add-build-id"
  addBuildId e p b r i
  liftIO . T.putStrLn $ "add-build-ref"
  addBuildRef e p b r
  liftIO . T.putStrLn $ "add-project-commit"
  addProjectCommit e p c
  liftIO . T.putStrLn $ "add-project-commit-build-id"
  addProjectCommitBuildId e p c i
  liftIO . T.putStrLn $ "add-set-ref"
  void . A.send $ D.updateItem (tBuild e)
    & D.uiKey .~ H.fromList [
        vBuildId i
      ]
    & D.uiUpdateExpression .~ Just (mconcat ["SET ", kRef, " = ", kVal "r", ", ", kCommit, " = ", kVal "c"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        vRefOf (kVal "r") r
      , vCommitOf (kVal "c") c
      ]

deindex :: Environment -> Project -> Build -> BuildId -> AWS ()
deindex e p b i = do
  clearQueued e p b i

acknowledge :: Environment -> BuildId -> GroupName -> StreamName -> AWS Acknowledge
acknowledge e i g s = do
  now <- liftIO getCurrentTime
  handling D._ConditionalCheckFailedException (const . pure $ AlreadyRunning) . fmap (const Accept) $ A.send $ D.updateItem (tBuild e)
    & D.uiKey .~ H.fromList [
        vBuildId i
      ]
    & D.uiUpdateExpression .~ Just (mconcat [
        "SET "
      , kStartTime, " = ", kVal "s", ", "
      , kLogGroup, " = ", kVal "lg", ", "
      , kLogStream, " = ", kVal "ls"
      ])
    & D.uiExpressionAttributeValues .~ H.fromList [
        vTime (kVal "s") now
      , vLogGroup (kVal "lg") g
      , vLogStream (kVal "ls") s
      ]
    & D.uiConditionExpression .~ (Just . mconcat $ ["attribute_not_exists(", kStartTime, ")"])

complete :: Environment -> BuildId -> BuildResult -> AWS ()
complete e i r = do
  now <- liftIO getCurrentTime
  void . A.send $ D.updateItem (tBuild e)
    & D.uiKey .~ H.fromList [
        vBuildId i
      ]
    & D.uiUpdateExpression .~ Just (mconcat ["SET ", kEndTime, " = ", kVal "t", ", ", kBuildResult, " = ", kVal "r"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        vTime (kVal "t") now
      , vBuildResult (kVal "r") r
      ]

heartbeat :: Environment -> BuildId -> AWS ()
heartbeat e i r = do
  now <- liftIO getCurrentTime
  void . A.send $ D.updateItem (tBuild e)
    & D.uiKey .~ H.fromList [
        vBuildId i
      ]
    & D.uiUpdateExpression .~ Just (mconcat ["SET ", kHeartbeatTime, " = ", kVal "t"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        vTime (kVal "t") now
      ]

delete :: Environment -> BuildId -> AWS ()
delete e i =
  void . A.send $ D.deleteItem (tBuild e)
    & D.diKey .~ H.fromList [
        vBuildId i
      ]

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
