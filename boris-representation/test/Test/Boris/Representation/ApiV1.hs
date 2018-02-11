{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Boris.Representation.ApiV1 where

import           Boris.Core.Data
import           Boris.Representation.ApiV1

import qualified Data.Aeson as Aeson

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           P

import           Test.Boris.Core.Gen

import           System.IO (IO)

prop_get_commit_response :: Property
prop_get_commit_response =
  property $ do
    project <- forAll genProject
    builds <- forAll $ Gen.list (Range.linear 0 100) genBuildId
    verify $ GetCommit project (sortBuildIds builds)

prop_get_queue_response :: Property
prop_get_queue_response =
  property $ do
    queue <- forAll genQueueSize
    verify $ GetQueue queue

prop_get_builds_response :: Property
prop_get_builds_response =
  property $ do
    tree <- forAll genBuildTree
    verify $ GetBuilds tree

prop_get_build_response :: Property
prop_get_build_response =
  property $ do
    build <- forAll genBuildData
    verify $ GetBuild build

prop_post_build_request :: Property
prop_post_build_request =
  property $ do
    ref <- forAll (Gen.maybe genRef)
    verify $ PostBuildRequest ref

prop_put_build_ignore_response :: Property
prop_put_build_ignore_response =
  property $ do
    ignore <- forAll Gen.bool
    verify $ PutBuildIgnore ignore

prop_get_projects_response :: Property
prop_get_projects_response =
  property $ do
    project <- forAll (Gen.list (Range.linear 0 100) genProject)
    verify $ GetProjects project

prop_get_project_response :: Property
prop_get_project_response =
  property $ do
    project <- forAll genProject
    builds <- forAll $ Gen.list (Range.linear 0 20) genBuild
    verify $ GetProject project builds

prop_get_scoreboard_response :: Property
prop_get_scoreboard_response =
  property $ do
    results <- forAll $ Gen.list (Range.linear 0 20) genResult
    verify $ GetScoreboard results

prop_post_discover_request :: Property
prop_post_discover_request =
  property $ do
    project <- forAll genProject
    discovery <- forAll $ Gen.list (Range.linear 1 10) $ do
      DiscoverInstance <$> genBuild <*> genRef <*> genCommit
    verify $ PostDiscover project discovery

prop_post_heartbeat_response :: Property
prop_post_heartbeat_response =
  property $ do
    cancelled <- forAll genBuildCancelled
    verify $ PostHeartbeatResponse cancelled

prop_post_acknowledge_response :: Property
prop_post_acknowledge_response =
  property $ do
    acknowledge <- forAll genAcknowledge
    verify $ PostAcknowledgeResponse acknowledge

prop_post_disavow_request :: Property
prop_post_disavow_request =
  property $ do
    project <- forAll genProject
    build <- forAll genBuild
    verify $ PostDisavowRequest project build

prop_post_avow_request :: Property
prop_post_avow_request =
  property $ do
    project <- forAll genProject
    build <- forAll genBuild
    ref <- forAll genRef
    commit <- forAll genCommit
    verify $ PostAvowRequest project build ref commit

verify :: (MonadTest m, Show a, Eq a, Aeson.ToJSON a, Aeson.FromJSON a) => a -> m ()
verify a =
  tripping a Aeson.encode Aeson.decode

tests :: IO Bool
tests =
  checkParallel $$(discover)
