{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Store.Dynamo.Build (
    cancel
  , delete
  , register
  , acknowledge
  , acknowledge'
  , complete
  , heartbeat
  , index
  , deindex
  , fetch
  ) where

import           Boris.Core.Data
import           Boris.Http.Store.Error
import           Boris.Http.Store.Dynamo.Schema
import           Boris.Http.Store.Dynamo.Index

import           Control.Lens (_Just, ix, to, (.~), (^?), (^.))
import           Control.Exception.Lens (handling)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Data.Time (UTCTime, getCurrentTime, parseTimeM)
import           Data.Time.Locale.Compat (defaultTimeLocale)
import qualified Data.HashMap.Strict as H

import           Jebediah.Data (LogGroup (..), LogStream (..))

import           Mismi (AWS)
import qualified Mismi.Amazonka as A

import qualified Network.AWS.DynamoDB as D

import           P

import           Spine.Data (TableName (..), renderKey, toEncoding, toItemEncoding, fromEncoding_)

import           X.Control.Monad.Trans.Either (EitherT, newEitherT)


fetch :: Environment -> BuildId -> EitherT FetchError AWS BuildData
fetch e i = newEitherT $ do
  res <- A.send $ D.getItem (renderTableName $ tBuild e)
    & D.giKey .~ H.fromList [
        toItemEncoding iBuildId (renderBuildId i)
      ]
    & D.giConsistentRead .~
      Just False
  pure $ BuildData i
    <$> (maybe (Left $ MissingProject i) Right $ res ^? D.girsItem . ix (renderKey kProject) . D.avS . _Just . to Project)
    <*> (maybe (Left $ MissingBuild i) Right $ res ^? D.girsItem . ix (renderKey kBuild) . D.avS . _Just . to Build)
    <*> (Right $ res ^? D.girsItem . ix (renderKey kRef) . D.avS . _Just . to Ref)
    <*> (Right $ res ^? D.girsItem . ix (renderKey kCommit) . D.avS . _Just . to Commit)
    <*> (forM (res ^? D.girsItem . ix (renderKey kQueueTime) . D.avS . _Just) $ fromMaybeM (Left $ InvalidQueueTime i) . blat)
    <*> (forM (res ^? D.girsItem . ix kStartTime . D.avS . _Just) $ fromMaybeM (Left $ InvalidStartTime i) . blat)
    <*> (forM (res ^? D.girsItem . ix kEndTime . D.avS . _Just) $ fromMaybeM (Left $ InvalidEndTime i) . blat)
    <*> (forM (res ^? D.girsItem . ix kHeartbeatTime . D.avS . _Just) $ fromMaybeM (Left $ InvalidHeartbeatTime i) . blat)
    <*> (Right . fmap (bool BuildKo BuildOk) $ res ^? D.girsItem . ix (renderKey kBuildResult) . D.avBOOL . _Just)
    <*> (Right $ LogData <$> res ^? D.girsItem . ix kLogGroup . D.avS . _Just . to LogGroup <*> res ^? D.girsItem . ix kLogStream . D.avS . _Just . to LogStream)
    <*> (Right . fmap (bool BuildNotCancelled BuildCancelled) $ res ^? D.girsItem . ix kCancelled . D.avBOOL . _Just)

blat :: Text -> Maybe UTCTime
blat =
  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" . T.unpack

register :: Environment -> Project -> Build -> BuildId -> EitherT RegisterError AWS ()
register e p b i = do
  now <- liftIO getCurrentTime
  newEitherT $ handling D._ConditionalCheckFailedException (const . pure . Left $ BuildIdAlreadyRegistered e p b i) . fmap (const $ Right ()) $ A.send $ D.putItem (renderTableName $ tBuild e)
    & D.piItem .~ H.fromList [
        toItemEncoding iBuildId (renderBuildId i)
      , toEncoding kQueueTime now
      , toEncoding kProject (renderProject p)
      , toEncoding kBuild (renderBuild b)
      ]
    & D.piConditionExpression .~ (Just . mconcat $ [
        "attribute_not_exists("
      , renderKey kProjectBuild
      , ") AND attribute_not_exists("
      , kBuildId
      , ")"
      ])
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
  liftIO . T.putStrLn $ "add-project-commit-seen"
  addProjectCommitSeen e p c b
  liftIO . T.putStrLn $ "add-set-ref"
  void . A.send $ D.updateItem (renderTableName $ tBuild e)
    & D.uiKey .~ H.fromList [
        toItemEncoding iBuildId (renderBuildId i)
      ]
    & D.uiUpdateExpression .~ Just (mconcat [
        "SET "
        , renderKey kRef, " = ", renderKey $ kVal "r", ", "
        , renderKey kCommit, " = ", renderKey $ kVal "c"
      ])
    & D.uiExpressionAttributeValues .~ H.fromList [
        toEncoding (kVal "r") (renderRef r)
      , toEncoding (kVal "c") (renderCommit c)
      ]

deindex :: Environment -> Project -> Build -> BuildId -> AWS ()
deindex e p b i = do
  clearQueued e p b i

acknowledge :: Environment -> BuildId -> LogGroup -> LogStream -> AWS Acknowledge
acknowledge e i g s = do
  now <- liftIO getCurrentTime
  handling D._ConditionalCheckFailedException (const . pure $ AlreadyRunning) . fmap (const Accept) $ A.send $ D.updateItem (renderTableName $ tBuild e)
    & D.uiKey .~ H.fromList [
        toItemEncoding iBuildId (renderBuildId i)
      ]
    & D.uiUpdateExpression .~ Just (mconcat [
        "SET "
      , kStartTime, " = ", renderKey $ kVal "s", ", "
      , kLogGroup, " = ", renderKey $ kVal "lg", ", "
      , kLogStream, " = ", renderKey $ kVal "ls"
      ])
    & D.uiExpressionAttributeValues .~ H.fromList [
        toEncoding (kTime "s") now
      , toEncoding (kVal "lg") (logGroup g)
      , toEncoding (kVal "ls") (logStream s)
      ]
    & D.uiConditionExpression .~ (Just . mconcat $ ["attribute_not_exists(", kStartTime, ")"])

acknowledge' :: Environment -> BuildId -> AWS Acknowledge
acknowledge' e i = do
  now <- liftIO getCurrentTime
  handling D._ConditionalCheckFailedException (const . pure $ AlreadyRunning) . fmap (const Accept) $ A.send $ D.updateItem (renderTableName $ tBuild e)
    & D.uiKey .~ H.fromList [
        toItemEncoding iBuildId (renderBuildId i)
      ]
    & D.uiUpdateExpression .~ Just (mconcat [
        "SET "
      , kStartTime, " = ", renderKey $ kVal "s", ", "
      ])
    & D.uiExpressionAttributeValues .~ H.fromList [
        toEncoding (kTime "s") now
      ]
    & D.uiConditionExpression .~ (Just . mconcat $ ["attribute_not_exists(", kStartTime, ")"])

complete :: Environment -> BuildId -> BuildResult -> AWS (Maybe Result)
complete e i r = do
  now <- liftIO getCurrentTime
  z <- A.send $ D.updateItem (renderTableName $ tBuild e)
    & D.uiKey .~ H.fromList [
        toItemEncoding iBuildId (renderBuildId i)
      ]
    & D.uiUpdateExpression .~ Just (mconcat [
        "SET "
      , kEndTime, " = ", renderKey $ kVal "t", ", "
      , renderKey kBuildResult, " = ", renderKey $ kVal "r"
      ])
    & D.uiReturnValues .~ Just D.AllNew
    & D.uiExpressionAttributeValues .~ H.fromList [
        toEncoding (kTime "t") now
      , toEncoding (kBool "r") (case r of BuildOk -> True; BuildKo -> False)
      ]

  pure $
    Result i
     <$> (Project <$> fromEncoding_ kProject (z ^. D.uirsAttributes))
     <*> (Build <$> fromEncoding_ kBuild (z ^. D.uirsAttributes))
     <*> pure (Ref <$> fromEncoding_ kRef (z ^. D.uirsAttributes))
     <*> pure r

heartbeat :: Environment -> BuildId -> AWS BuildCancelled
heartbeat e i = do
  now <- liftIO getCurrentTime
  void . A.send $ D.updateItem (renderTableName $ tBuild e)
    & D.uiKey .~ H.fromList [
        toItemEncoding iBuildId (renderBuildId i)
      ]
    & D.uiUpdateExpression .~ Just (mconcat ["SET ", kHeartbeatTime, " = ", renderKey $ kTime "t"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        toEncoding (kTime "t") now
      ]
  res <- A.send $ D.getItem (renderTableName $ tBuild e)
    & D.giKey .~ H.fromList [
        toItemEncoding iBuildId (renderBuildId i)
      ]
    & D.giConsistentRead .~
      Just False
  pure . maybe BuildNotCancelled (bool BuildNotCancelled BuildCancelled) . join $ res ^? D.girsItem . ix kCancelled . D.avBOOL

cancel :: Environment -> BuildId -> AWS Bool
cancel e i = do
  handling D._ConditionalCheckFailedException (const . pure $ False) . fmap (const True) $
    A.send $ D.updateItem (renderTableName $ tBuild e)
      & D.uiKey .~ H.fromList [
          toItemEncoding iBuildId (renderBuildId i)
        ]
      & D.uiUpdateExpression .~ Just (mconcat ["SET ", kCancelled, " = ", renderKey $ kBool "c"])
      & D.uiExpressionAttributeValues .~ H.fromList [
          toEncoding (kBool "c") True
        ]
      & D.uiConditionExpression .~ (Just . mconcat $ ["attribute_exists(", kBuildId, ")"])

delete :: Environment -> BuildId -> AWS ()
delete e i =
  void . A.send $ D.deleteItem (renderTableName $ tBuild e)
    & D.diKey .~ H.fromList [
        toItemEncoding iBuildId (renderBuildId i)
      ]
