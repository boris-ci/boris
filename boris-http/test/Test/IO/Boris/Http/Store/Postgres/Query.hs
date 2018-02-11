{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Boris.Http.Store.Postgres.Query  where

import           Boris.Core.Data
import qualified Boris.Http.Store.Postgres.Query as Query

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           Jebediah.Data (LogGroup (..), LogStream (..))

import           P

import           System.IO (IO)

import qualified Test.Boris.Core.Gen as Gen
import           Test.IO.Boris.Http.Store.Postgres.Test

prop_fetch :: Property
prop_fetch =
  property $ do
    project <- forAll Gen.genProject
    build <- forAll Gen.genBuild
    buildid <- forAll Gen.genBuildId
    actual <- db $ do
      Query.register project build buildid
      result <- Query.fetch buildid

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
      Query.register project build buildid
      Query.cancel buildid
      result <- Query.fetch buildid
      pure $ result >>= buildDataCancelled
    actual === Just BuildCancelled

prop_acknowledge :: Property
prop_acknowledge =
  property $ do
    project <- forAll Gen.genProject
    build <- forAll Gen.genBuild
    buildid <- forAll Gen.genBuildId
    group <- LogGroup <$> forAll (Gen.element ["fred", "barney"])
    stream <- LogStream <$> forAll (Gen.element ["1", "2", "3"])
    actual <- db $ do
      Query.register project build buildid
      ack <- Query.acknowledge buildid group stream
      result <- Query.fetch buildid
      pure $ (ack, isJust . buildDataStartTime <$> result, isJust . buildDataLog <$> result)
    actual === (Accept, Just True, Just True)

prop_acknowledge_reject :: Property
prop_acknowledge_reject =
  property $ do
    project <- forAll Gen.genProject
    build <- forAll Gen.genBuild
    buildid <- forAll Gen.genBuildId
    group <- LogGroup <$> forAll (Gen.element ["fred", "barney"])
    stream <- LogStream <$> forAll (Gen.element ["1", "2", "3"])
    actual <- db $ do
      Query.register project build buildid
      ack1 <- Query.acknowledge buildid group stream
      ack2 <- Query.acknowledge buildid group stream
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
      Query.register project build buildid
      ref <- Query.complete buildid resultx
      resultxx <- Query.fetch buildid
      pure $ (ref, isJust $ resultxx >>= buildDataEndTime, resultxx >>= buildDataResult)
    actual === (Nothing, True, Just resultx)

prop_heartbeat :: Property
prop_heartbeat =
  property $ do
    project <- forAll Gen.genProject
    build <- forAll Gen.genBuild
    buildid <- forAll Gen.genBuildId
    actual <- db $ do
      Query.register project build buildid
      a <- Query.heartbeat buildid
      b <- Query.heartbeat buildid
      Query.cancel buildid
      c <- Query.heartbeat buildid
      result <- Query.fetch buildid
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
      Query.register project build buildid
      Query.index buildid ref commit
      result <- Query.fetch buildid
      pure $
        (buildDataProject <$> result, buildDataBuild <$> result, buildDataId <$> result, result >>= buildDataRef, result >>= buildDataCommit)
    actual === (Just project, Just build, Just buildid, Just ref, Just commit)

tests :: IO Bool
tests =
  checkDb $$(discover)
