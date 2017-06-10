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
  , addProjectCommitDiscovered
  , clearQueued
  , getProjects
  , getProjectRefs
  , getProjectCommits
  , getProjectCommitBuildIds
  , getProjectCommitSeen
  , getProjectCommitDiscovered
  , getBuildIds
  , getQueued
  , getBuildRefs
  , setBuildDisabled
  , isBuildDisabled
  , deleteProjects
  , deleteProjectRefs
  , deleteBuildIds
  , deleteQueued
  , deleteBuildRefs
  , deleteProjectCommit
  ) where

import           Boris.Core.Data
import           Boris.Store.Schema

import           Control.Lens (ix, (.~), (^?))

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T


import           Mismi (AWS)
import qualified Mismi.Amazonka as A

import qualified Network.AWS.DynamoDB as D

import           P

import           Spine.Data (TableName (..), renderKey, toEncoding)


addProject :: Environment -> Project -> Build -> AWS ()
addProject e p b = do
  void . A.send $ D.updateItem (renderTableName $ tProject e)
    & D.uiKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      ]
    & D.uiUpdateExpression .~
      Just (mconcat ["ADD ", kBuilds, " ", renderKey $ kValSet "v"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        toEncoding (kValSet "v") [renderBuild b]
      ]


addProjectRef :: Environment -> Project -> Ref -> Build -> AWS ()
addProjectRef e p r b = do
  void . A.send $ D.updateItem (renderTableName $ tProjectRefs e)
    & D.uiKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      , toEncoding kRef (renderRef r)
      ]
    & D.uiUpdateExpression .~
      Just (mconcat ["ADD ", kBuilds, " ", renderKey $ kValSet "v"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        toEncoding (kValSet "v") [renderBuild b]
      ]

addBuildId :: Environment -> Project -> Build -> Ref -> BuildId -> AWS ()
addBuildId e p b r i = do
  void . A.send $ D.updateItem (renderTableName $ tRefs e)
    & D.uiKey .~ H.fromList [
        toEncoding kProjectBuild (renderProjectBuild p b)
      , toEncoding kRef (renderRef r)
      ]
    & D.uiUpdateExpression .~
      Just (mconcat ["ADD ", kBuildIds, " ", renderKey $ kValSet "v"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        toEncoding (kValSet "v") [renderBuildId i]
      ]

addQueued :: Environment -> Project -> Build -> BuildId -> AWS ()
addQueued e p b i = do
  void . A.send $ D.updateItem (renderTableName $ tBuilds e)
    & D.uiKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      , toEncoding kBuild (renderBuild b)
      ]
    & D.uiUpdateExpression .~
      Just (mconcat ["ADD ", kQueued, " ", renderKey $ kValSet "v"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        toEncoding (kValSet "v") [renderBuildId i]
      ]

addBuildRef :: Environment -> Project -> Build -> Ref -> AWS ()
addBuildRef e p b r = do
  void . A.send $ D.updateItem (renderTableName $ tBuilds e)
    & D.uiKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      , toEncoding kBuild (renderBuild b)
      ]
    & D.uiUpdateExpression .~
      Just (T.intercalate " " [
          "ADD", kRefs, renderKey $ kValSet "v"
        , "REMOVE", kDisabled
        ])
    & D.uiExpressionAttributeValues .~ H.fromList [
        toEncoding (kValSet "v") [renderRef r]
      ]

addProjectCommit :: Environment -> Project -> Commit -> AWS ()
addProjectCommit e p c = do
  void . A.send $ D.updateItem (renderTableName $ tProject e)
    & D.uiKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      ]
    & D.uiUpdateExpression .~
      Just (mconcat ["ADD ", kCommits, " ", renderKey $ kValSet "v"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        toEncoding (kValSet "v") [renderCommit c]
      ]

addProjectCommitBuildId :: Environment -> Project -> Commit -> BuildId -> AWS ()
addProjectCommitBuildId e p c i = do
  void . A.send $ D.updateItem (renderTableName $ tProjectCommits e)
    & D.uiKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      , toEncoding kCommit (renderCommit c)
      ]
    & D.uiUpdateExpression .~
      Just (mconcat ["ADD ", kBuilds, " ", renderKey $ kValSet "v"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        toEncoding (kValSet "v") [renderBuildId i]
      ]

addProjectCommitSeen :: Environment -> Project -> Commit -> Build -> AWS ()
addProjectCommitSeen e p c b = do
  void . A.send $ D.updateItem (renderTableName $ tProjectCommits e)
    & D.uiKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      , toEncoding kCommit (renderCommit c)
      ]
    & D.uiUpdateExpression .~
      Just (mconcat ["ADD ", kSeen, " ", renderKey $ kValSet "v"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        toEncoding (kValSet "v") [renderBuild b]
      ]

addProjectCommitDiscovered :: Environment -> Project -> Commit -> Build -> AWS ()
addProjectCommitDiscovered e p c b = do
  void . A.send $ D.updateItem (renderTableName $ tProjectCommits e)
    & D.uiKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      , toEncoding kCommit (renderCommit c)
      ]
    & D.uiUpdateExpression .~
      Just (mconcat ["ADD ", kDiscovered, " ", renderKey $ kValSet "v"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        toEncoding (kValSet "v") [renderBuild b]
      ]

clearQueued :: Environment -> Project -> Build -> BuildId -> AWS ()
clearQueued e p b i = do
  void . A.send $ D.updateItem (renderTableName $ tBuilds e)
    & D.uiKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      , toEncoding kBuild (renderBuild b)
      ]
    & D.uiUpdateExpression .~
      Just (mconcat ["DELETE ", kQueued, " ", renderKey $ kValSet "v"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        toEncoding (kValSet "v") [renderBuildId i]
      ]

getProjects :: Environment -> Project -> AWS [Build]
getProjects e p = do
  res <- A.send $ D.getItem (renderTableName $ tProject e)
    & D.giKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      ]
    & D.giConsistentRead .~
      Just False
  pure . fmap Build . fromMaybe [] $ res ^? D.girsItem . ix kBuilds . D.avSS

getProjectCommits :: Environment -> Project -> AWS [Commit]
getProjectCommits e p = do
  res <- A.send $ D.getItem (renderTableName $ tProject e)
    & D.giKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      ]
    & D.giConsistentRead .~
      Just False
  pure . fmap Commit . fromMaybe [] $ res ^? D.girsItem . ix kCommits . D.avSS

getProjectRefs :: Environment -> Project -> Ref -> AWS [Build]
getProjectRefs e p r = do
  res <- A.send $ D.getItem (renderTableName $ tProjectRefs e)
    & D.giKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      , toEncoding kRef (renderRef r)
      ]
    & D.giConsistentRead .~
      Just False
  pure . fmap Build . fromMaybe [] $ res ^? D.girsItem . ix kBuilds . D.avSS

getProjectCommitBuildIds :: Environment -> Project -> Commit -> AWS [BuildId]
getProjectCommitBuildIds e p c = do
  res <- A.send $ D.getItem (renderTableName $ tProjectCommits e)
    & D.giKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      , toEncoding kCommit (renderCommit c)
      ]
    & D.giConsistentRead .~
      Just False
  pure . fmap BuildId . fromMaybe [] $ res ^? D.girsItem . ix kBuilds . D.avSS

getProjectCommitSeen :: Environment -> Project -> Commit -> AWS [Build]
getProjectCommitSeen e p c = do
  res <- A.send $ D.getItem (renderTableName $ tProjectCommits e)
    & D.giKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      , toEncoding kCommit (renderCommit c)
      ]
    & D.giConsistentRead .~
      Just False
  pure . fmap Build . fromMaybe [] $ res ^? D.girsItem . ix kSeen . D.avSS

getProjectCommitDiscovered :: Environment -> Project -> Commit -> AWS [Build]
getProjectCommitDiscovered e p c = do
  res <- A.send $ D.getItem (renderTableName $ tProjectCommits e)
    & D.giKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      , toEncoding kCommit (renderCommit c)
      ]
    & D.giConsistentRead .~
      Just False
  pure . fmap Build . fromMaybe [] $ res ^? D.girsItem . ix kDiscovered . D.avSS

getBuildIds :: Environment -> Project -> Build -> Ref -> AWS [BuildId]
getBuildIds e p b r = do
  res <- A.send $ D.getItem (renderTableName $ tRefs e)
    & D.giKey .~ H.fromList [
        toEncoding kProjectBuild (renderProjectBuild p b)
      , toEncoding kRef (renderRef r)
      ]
    & D.giConsistentRead .~
      Just False
  pure . fmap BuildId . fromMaybe [] $ res ^? D.girsItem . ix kBuildIds . D.avSS

getQueued :: Environment -> Project -> Build -> AWS [BuildId]
getQueued e p b = do
  res <- A.send $ D.getItem (renderTableName $ tBuilds e)
    & D.giKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      , toEncoding kBuild (renderBuild b)
      ]
    & D.giConsistentRead .~
      Just False
  pure . fmap BuildId . fromMaybe [] $ res ^? D.girsItem . ix kQueued . D.avSS

getBuildRefs :: Environment -> Project -> Build -> AWS [Ref]
getBuildRefs e p b = do
  res <- A.send $ D.getItem (renderTableName $ tBuilds e)
    & D.giKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      , toEncoding kBuild (renderBuild b)
      ]
    & D.giConsistentRead .~
      Just False
  pure . fmap Ref . fromMaybe [] $ res ^? D.girsItem . ix kRefs . D.avSS

setBuildDisabled :: Environment -> Project -> Build -> Bool -> AWS ()
setBuildDisabled e p b d = do
  void . A.send $ D.updateItem (renderTableName $ tBuilds e)
    & D.uiKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      , toEncoding kBuild (renderBuild b)
      ]
    & D.uiUpdateExpression .~
      Just (mconcat ["SET ", kDisabled, " = ", renderKey $ kBool "v"])
    & D.uiExpressionAttributeValues .~ H.fromList [
        toEncoding (kBool "v") d
      ]

isBuildDisabled :: Environment -> Project -> Build -> AWS Bool
isBuildDisabled e p b = do
  res <- A.send $ D.getItem (renderTableName $ tBuilds e)
    & D.giKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      , toEncoding kBuild (renderBuild b)
      ]
    & D.giConsistentRead .~
      Just False
  pure . maybe False (fromMaybe False) $ res ^? D.girsItem . ix kDisabled . D.avBOOL


deleteProjects :: Environment -> Project -> AWS ()
deleteProjects e p =
  void . A.send $ D.deleteItem (renderTableName $ tProject e)
    & D.diKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      ]

deleteProjectRefs :: Environment -> Project -> Ref -> AWS ()
deleteProjectRefs e p r =
  void . A.send $ D.deleteItem (renderTableName $ tProjectRefs e)
    & D.diKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      , toEncoding kRef (renderRef r)
      ]

deleteBuildIds :: Environment -> Project -> Build -> Ref -> AWS ()
deleteBuildIds e p b r =
  void . A.send $ D.deleteItem (renderTableName $ tRefs e)
    & D.diKey .~ H.fromList [
        toEncoding kProjectBuild (renderProjectBuild p b)
      , toEncoding kRef (renderRef r)
      ]

deleteQueued :: Environment -> Project -> Build -> AWS ()
deleteQueued e p b =
  void . A.send $ D.deleteItem (renderTableName $ tBuilds e)
    & D.diKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      , toEncoding kBuild (renderBuild b)
      ]

deleteBuildRefs :: Environment -> Project -> Build -> AWS ()
deleteBuildRefs e p b =
  void . A.send $ D.deleteItem (renderTableName $ tBuilds e)
    & D.diKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      , toEncoding kBuild (renderBuild b)
      ]

deleteProjectCommit :: Environment -> Project -> Commit -> AWS ()
deleteProjectCommit e p c =
  void . A.send $ D.deleteItem (renderTableName $ tProjectCommits e)
    & D.diKey .~ H.fromList [
        toEncoding kProject (renderProject p)
      , toEncoding kCommit (renderCommit c)
      ]
