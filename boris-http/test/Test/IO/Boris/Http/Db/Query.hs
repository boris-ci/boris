{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Boris.Http.Db.Query  where

import           Boris.Core.Data
import           Boris.Http.Data
import qualified Boris.Http.Db.Query as Query

import           Control.Monad.IO.Class (MonadIO (..))

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           Jebediah.Data (LogGroup (..), LogStream (..))

import           P

import           System.IO (IO)

import qualified Test.Boris.Core.Gen as Gen
import qualified Test.Boris.Http.Gen as Gen
import           Test.IO.Boris.Http.Db.Test

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
      pure $ (ack, isJust . buildDataStartTime <$> result)
    actual === (Accept, Just True)

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

prop_user :: Property
prop_user =
  property $ do
    github1 <- forAll Gen.genGithubUser
    github2 <- forAll Gen.genGithubUser
    actual <- db $ do
      user1 <- Query.addUser github1
      muser1 <- Query.userByGithubId (githubUserId github1)
      Query.updateUser (user1 { userGithub = github2 })
      muser2 <- Query.userByGithubId (githubUserId github2)
      pure (userGithub user1, userGithub <$> muser1, userGithub <$> muser2)
    actual === (github1, Just github1, Just github2)

prop_session :: Property
prop_session =
  property $ do
    sessionId <- liftIO newSessionToken
    github <- forAll Gen.genGithubUser
    oauth <- forAll Gen.genGithubOAuth
    let session = Session sessionId oauth
    (user, actual) <- db $ do
      user <- Query.addUser github
      Query.newSession session user
      Query.tickSession sessionId
      result <- Query.getSession sessionId
      pure (user, result)
    actual === (Just $ AuthenticatedUser user $ Session sessionId oauth)

tests :: IO Bool
tests =
  checkDb $$(discover)
