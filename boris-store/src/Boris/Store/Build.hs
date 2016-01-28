{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Store.Build (
    RegisterError (..)
  , renderRegisterError
  , delete
  , register
  , acknowledge
  , complete
  ) where

import           Boris.Core.Data
import           Boris.Store.Schema

import           Control.Lens ((&), (.~))
import           Control.Exception.Lens (handling)
import           Control.Monad.IO.Class (liftIO)

import           Data.Text (Text)

import           Data.Time (getCurrentTime)
import qualified Data.HashMap.Strict as H


import           Mismi (AWS)
import qualified Mismi.Amazonka as A

import qualified Network.AWS.DynamoDB as D

import           P

import           X.Control.Monad.Trans.Either (EitherT, newEitherT)


data RegisterError =
    BuildIdAlreadyRegistered Environment Project Build BuildId
    deriving (Eq, Show)

register :: Environment -> Project -> Build -> BuildId -> EitherT RegisterError AWS ()
register e p b i = newEitherT $ do
  now <- liftIO getCurrentTime
  handling D._ConditionalCheckFailedException (const . pure . Left $ BuildIdAlreadyRegistered e p b i) . fmap (const $ Right ()) $ A.send $ D.putItem (tBuild e)
    & D.piItem .~ H.fromList [
        vProjectBuild p b
      , vBuildId i
      , vTime kQueueTime now
      ]
    & D.piConditionExpression .~ (Just . mconcat $ ["attribute_not_exists(", kProjectBuild, ") AND attribute_not_exists(", kBuildId, ")"])


acknowledge :: Environment -> Project -> Build -> BuildId -> AWS Acknowledge
acknowledge e p b i = do
  now <- liftIO getCurrentTime
  handling D._ConditionalCheckFailedException (const . pure $ AlreadyRunning) . fmap (const Accept) $ A.send $ D.updateItem (tBuild e)
    & D.uiKey .~ H.fromList [
        vProjectBuild p b
      , vBuildId i
      ]
    & D.uiUpdateExpression .~ Just (mconcat ["SET ", kStartTime, " = ", kVal "s"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        vTime (kVal "s") now
      ]
    & D.uiConditionExpression .~ (Just . mconcat $ ["attribute_not_exists(", kStartTime, ")"])

complete :: Environment -> Project -> Build -> BuildId -> BuildResult -> AWS ()
complete e p b i r = do
  now <- liftIO getCurrentTime
  void . A.send $ D.updateItem (tBuild e)
    & D.uiKey .~ H.fromList [
        vProjectBuild p b
      , vBuildId i
      ]
    & D.uiUpdateExpression .~ Just (mconcat ["SET ", kEndTime, " = ", kVal "t", ", ", kBuildResult, " = ", kVal "r"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        vTime (kVal "t") now
      , vBuildResult (kVal "r") r
      ]

delete :: Environment -> Project -> Build -> BuildId -> AWS ()
delete e p b i =
  void . A.send $ D.deleteItem (tBuild e)
    & D.diKey .~ H.fromList [
        vProjectBuild p b
      , vBuildId i
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
