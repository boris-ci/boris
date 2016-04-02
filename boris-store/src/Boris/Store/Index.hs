{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Store.Index (
    addProject
  , addProjectRef
  , addBuildId
  , addQueued
  , addBuildRef
  , addProjectCommit
  , addProjectCommitBuildId
  , addProjectCommitSeen
  , clearQueued
  , getProjects
  , getProjectRefs
  , getProjectCommits
  , getProjectCommitBuildIds
  , getProjectCommitSeen
  , getBuildIds
  , getQueued
  , getBuildRefs
  , deleteProjects
  , deleteProjectRefs
  , deleteBuildIds
  , deleteQueued
  , deleteBuildRefs
  , deleteProjectCommit
  ) where

import           Boris.Core.Data
import           Boris.Store.Schema

import           Control.Lens (ix, (&), (.~), (^?))

import qualified Data.HashMap.Strict as H


import           Mismi (AWS)
import qualified Mismi.Amazonka as A

import qualified Network.AWS.DynamoDB as D

import           P


addProject :: Environment -> Project -> Build -> AWS ()
addProject e p b = do
  void . A.send $ D.updateItem (tProject e)
    & D.uiKey .~ H.fromList [
        vProject p
      ]
    & D.uiUpdateExpression .~
      Just (mconcat ["ADD ", kBuilds, " ", kVal "v"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        vStrings (kVal "v") [renderBuild b]
      ]


addProjectRef :: Environment -> Project -> Ref -> Build -> AWS ()
addProjectRef e p r b = do
  void . A.send $ D.updateItem (tProjectRefs e)
    & D.uiKey .~ H.fromList [
        vProject p
      , vRef r
      ]
    & D.uiUpdateExpression .~
      Just (mconcat ["ADD ", kBuilds, " ", kVal "v"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        vStrings (kVal "v") [renderBuild b]
      ]

addBuildId :: Environment -> Project -> Build -> Ref -> BuildId -> AWS ()
addBuildId e p b r i = do
  void . A.send $ D.updateItem (tRefs e)
    & D.uiKey .~ H.fromList [
        vProjectBuild p b
      , vRef r
      ]
    & D.uiUpdateExpression .~
      Just (mconcat ["ADD ", kBuildIds, " ", kVal "v"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        vStrings (kVal "v") [renderBuildId i]
      ]

addQueued :: Environment -> Project -> Build -> BuildId -> AWS ()
addQueued e p b i = do
  void . A.send $ D.updateItem (tBuilds e)
    & D.uiKey .~ H.fromList [
        vProject p
      , vBuild b
      ]
    & D.uiUpdateExpression .~
      Just (mconcat ["ADD ", kQueued, " ", kVal "v"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        vStrings (kVal "v") [renderBuildId i]
      ]

addBuildRef :: Environment -> Project -> Build -> Ref -> AWS ()
addBuildRef e p b r = do
  void . A.send $ D.updateItem (tBuilds e)
    & D.uiKey .~ H.fromList [
        vProject p
      , vBuild b
      ]
    & D.uiUpdateExpression .~
      Just (mconcat ["ADD ", kRefs, " ", kVal "v"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        vStrings (kVal "v") [renderRef r]
      ]

addProjectCommit :: Environment -> Project -> Commit -> AWS ()
addProjectCommit e p c = do
  void . A.send $ D.updateItem (tProject e)
    & D.uiKey .~ H.fromList [
        vProject p
      ]
    & D.uiUpdateExpression .~
      Just (mconcat ["ADD ", kCommits, " ", kVal "v"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        vStrings (kVal "v") [renderCommit c]
      ]

addProjectCommitBuildId :: Environment -> Project -> Commit -> BuildId -> AWS ()
addProjectCommitBuildId e p c i = do
  void . A.send $ D.updateItem (tProjectCommits e)
    & D.uiKey .~ H.fromList [
        vProject p
      , vCommit c
      ]
    & D.uiUpdateExpression .~
      Just (mconcat ["ADD ", kBuilds, " ", kVal "v"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        vStrings (kVal "v") [renderBuildId i]
      ]

addProjectCommitSeen :: Environment -> Project -> Commit -> Build -> AWS ()
addProjectCommitSeen e p c b = do
  void . A.send $ D.updateItem (tProjectCommits e)
    & D.uiKey .~ H.fromList [
        vProject p
      , vCommit c
      ]
    & D.uiUpdateExpression .~
      Just (mconcat ["ADD ", kSeen, " ", kVal "v"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        vStrings (kVal "v") [renderBuild b]
      ]

clearQueued :: Environment -> Project -> Build -> BuildId -> AWS ()
clearQueued e p b i = do
  void . A.send $ D.updateItem (tBuilds e)
    & D.uiKey .~ H.fromList [
        vProject p
      , vBuild b
      ]
    & D.uiUpdateExpression .~
      Just (mconcat ["DELETE ", kQueued, " ", kVal "v"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        vStrings (kVal "v") [renderBuildId i]
      ]

getProjects :: Environment -> Project -> AWS [Build]
getProjects e p = do
  res <- A.send $ D.getItem (tProject e)
    & D.giKey .~ H.fromList [
        vProject p
      ]
    & D.giConsistentRead .~
      Just False
  pure . fmap Build . fromMaybe [] $ res ^? D.girsItem . ix kBuilds . D.avSS

getProjectCommits :: Environment -> Project -> AWS [Commit]
getProjectCommits e p = do
  res <- A.send $ D.getItem (tProject e)
    & D.giKey .~ H.fromList [
        vProject p
      ]
    & D.giConsistentRead .~
      Just False
  pure . fmap Commit . fromMaybe [] $ res ^? D.girsItem . ix kCommits . D.avSS

getProjectRefs :: Environment -> Project -> Ref -> AWS [Build]
getProjectRefs e p r = do
  res <- A.send $ D.getItem (tProjectRefs e)
    & D.giKey .~ H.fromList [
        vProject p
      , vRef r
      ]
    & D.giConsistentRead .~
      Just False
  pure . fmap Build . fromMaybe [] $ res ^? D.girsItem . ix kBuilds . D.avSS

getProjectCommitBuildIds :: Environment -> Project -> Commit -> AWS [BuildId]
getProjectCommitBuildIds e p c = do
  res <- A.send $ D.getItem (tProjectCommits e)
    & D.giKey .~ H.fromList [
        vProject p
      , vCommit c
      ]
    & D.giConsistentRead .~
      Just False
  pure . fmap BuildId . fromMaybe [] $ res ^? D.girsItem . ix kBuilds . D.avSS

getProjectCommitSeen :: Environment -> Project -> Commit -> AWS [Build]
getProjectCommitSeen e p c = do
  res <- A.send $ D.getItem (tProjectCommits e)
    & D.giKey .~ H.fromList [
        vProject p
      , vCommit c
      ]
    & D.giConsistentRead .~
      Just False
  pure . fmap Build . fromMaybe [] $ res ^? D.girsItem . ix kSeen . D.avSS

getBuildIds :: Environment -> Project -> Build -> Ref -> AWS [BuildId]
getBuildIds e p b r = do
  res <- A.send $ D.getItem (tRefs e)
    & D.giKey .~ H.fromList [
        vProjectBuild p b
      , vRef r
      ]
    & D.giConsistentRead .~
      Just False
  pure . fmap BuildId . fromMaybe [] $ res ^? D.girsItem . ix kBuildIds . D.avSS

getQueued :: Environment -> Project -> Build -> AWS [BuildId]
getQueued e p b = do
  res <- A.send $ D.getItem (tBuilds e)
    & D.giKey .~ H.fromList [
        vProject p
      , vBuild b
      ]
    & D.giConsistentRead .~
      Just False
  pure . fmap BuildId . fromMaybe [] $ res ^? D.girsItem . ix kQueued . D.avSS

getBuildRefs :: Environment -> Project -> Build -> AWS [Ref]
getBuildRefs e p b = do
  res <- A.send $ D.getItem (tBuilds e)
    & D.giKey .~ H.fromList [
        vProject p
      , vBuild b
      ]
    & D.giConsistentRead .~
      Just False
  pure . fmap Ref . fromMaybe [] $ res ^? D.girsItem . ix kRefs . D.avSS


deleteProjects :: Environment -> Project -> AWS ()
deleteProjects e p =
  void . A.send $ D.deleteItem (tProject e)
    & D.diKey .~ H.fromList [
        vProject p
      ]

deleteProjectRefs :: Environment -> Project -> Ref -> AWS ()
deleteProjectRefs e p r =
  void . A.send $ D.deleteItem (tProjectRefs e)
    & D.diKey .~ H.fromList [
        vProject p
      , vRef r
      ]

deleteBuildIds :: Environment -> Project -> Build -> Ref -> AWS ()
deleteBuildIds e p b r =
  void . A.send $ D.deleteItem (tRefs e)
    & D.diKey .~ H.fromList [
        vProjectBuild p b
      , vRef r
      ]

deleteQueued :: Environment -> Project -> Build -> AWS ()
deleteQueued e p b =
  void . A.send $ D.deleteItem (tBuilds e)
    & D.diKey .~ H.fromList [
        vProject p
      , vBuild b
      ]

deleteBuildRefs :: Environment -> Project -> Build -> AWS ()
deleteBuildRefs e p b =
  void . A.send $ D.deleteItem (tBuilds e)
    & D.diKey .~ H.fromList [
        vProject p
      , vBuild b
      ]

deleteProjectCommit :: Environment -> Project -> Commit -> AWS ()
deleteProjectCommit e p c =
  void . A.send $ D.deleteItem (tProjectCommits e)
    & D.diKey .~ H.fromList [
        vProject p
      , vCommit c
      ]
