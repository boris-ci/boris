{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Boris.Http.Db.Build  where

import           Boris.Core.Data.Build
import qualified Boris.Http.Db.Build as BuildDb

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           P

import           System.IO (IO)

import qualified Test.Boris.Core.Gen as Gen
import           Test.IO.Boris.Http.Db.Test

prop_fetch :: Property
prop_fetch =
  property $ do
    project <- forAll Gen.genProject
    build <- forAll Gen.genBuild
    buildid <- forAll Gen.genBuildId
    actual <- db $ do
      BuildDb.register project build buildid
      result <- BuildDb.fetch buildid

      pure $
        (buildDataProject <$> result, buildDataBuild <$> result, buildDataId <$> result)
    actual === (Just project, Just build, Just buildid)

prop_cancel :: Property
prop_cancel =
  property $ do
    project <- forAll Gen.genProject
    build <- forAll Gen.genBuild
    buildid <- forAll Gen.genBuildId
    actual <- db $ do
      BuildDb.register project build buildid
      BuildDb.cancel buildid
      result <- BuildDb.fetch buildid
      pure $ result >>= buildDataCancelled
    actual === Just BuildCancelled

prop_acknowledge :: Property
prop_acknowledge =
  property $ do
    project <- forAll Gen.genProject
    build <- forAll Gen.genBuild
    buildid <- forAll Gen.genBuildId
    actual <- db $ do
      BuildDb.register project build buildid
      ack <- BuildDb.acknowledge buildid
      result <- BuildDb.fetch buildid
      pure $ (ack, isJust . buildDataStartTime <$> result)
    actual === (Accept, Just True)

prop_acknowledge_reject :: Property
prop_acknowledge_reject =
  property $ do
    project <- forAll Gen.genProject
    build <- forAll Gen.genBuild
    buildid <- forAll Gen.genBuildId
    actual <- db $ do
      BuildDb.register project build buildid
      ack1 <- BuildDb.acknowledge buildid
      ack2 <- BuildDb.acknowledge buildid
      pure $ (ack1, ack2)
    actual === (Accept, AlreadyRunning)

prop_complete :: Property
prop_complete =
  property $ do
    project <- forAll Gen.genProject
    build <- forAll Gen.genBuild
    buildid <- forAll Gen.genBuildId
    resultx <- forAll (Gen.element [BuildOk, BuildKo])
    actual <- db $ do
      BuildDb.register project build buildid
      ref <- BuildDb.complete buildid resultx
      resultxx <- BuildDb.fetch buildid
      pure $ (ref, isJust $ resultxx >>= buildDataEndTime, resultxx >>= buildDataResult)
    actual === (Nothing, True, Just resultx)

prop_heartbeat :: Property
prop_heartbeat =
  property $ do
    project <- forAll Gen.genProject
    build <- forAll Gen.genBuild
    buildid <- forAll Gen.genBuildId
    actual <- db $ do
      BuildDb.register project build buildid
      a <- BuildDb.heartbeat buildid
      b <- BuildDb.heartbeat buildid
      BuildDb.cancel buildid
      c <- BuildDb.heartbeat buildid
      result <- BuildDb.fetch buildid
      pure $ (a, b, c, isJust $ result >>= buildDataHeartbeatTime, result >>= buildDataCancelled)
    actual === (BuildNotCancelled, BuildNotCancelled, BuildCancelled, True, Just BuildCancelled)

prop_index :: Property
prop_index =
  property $ do
    project <- forAll Gen.genProject
    build <- forAll Gen.genBuild
    buildid <- forAll Gen.genBuildId
    ref <- forAll Gen.genRef
    commit <- forAll Gen.genCommit
    actual <- db $ do
      BuildDb.register project build buildid
      BuildDb.index buildid ref commit
      result <- BuildDb.fetch buildid
      pure $
        (buildDataProject <$> result, buildDataBuild <$> result, buildDataId <$> result, result >>= buildDataRef, result >>= buildDataCommit)
    actual === (Just project, Just build, Just buildid, Just ref, Just commit)

tests :: IO Bool
tests =
  checkDb $$(discover)
