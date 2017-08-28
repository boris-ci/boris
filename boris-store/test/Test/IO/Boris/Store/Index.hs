{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Boris.Store.Index where

import qualified Boris.Store.Index as SI

import           Control.Monad.IO.Class (MonadIO (..))
import qualified Control.Retry as Retry

import qualified Data.List as L

import           P

import           Test.Boris.Core.Arbitrary ()
import           Test.IO.Boris.Store
import           Test.QuickCheck
import           Test.Mismi (testAWS)

prop_project p b = once . testAWS . withClean environment (SI.deleteProjects environment p) $ do
  SI.addProject environment p b
  res <- retryOn null $
    SI.getProjects environment p
  pure $ res === [b]

prop_projectRef p r b = once . testAWS . withClean environment (SI.deleteProjectRefs environment p r) $ do
  SI.addProjectRef environment p r b
  res <- retryOn null $
    SI.getProjectRefs environment p r
  pure $ res === [b]

prop_projectCommit p c = once . testAWS . withClean environment (SI.deleteProjects environment p) $ do
  SI.addProjectCommit environment p c
  res <- retryOn null $
    SI.getProjectCommits environment p
  pure $ res === [c]

prop_projectCommitBuildId p c i = once . testAWS . withClean environment (SI.deleteProjectCommit environment p c) $ do
  SI.addProjectCommitBuildId environment p c i
  res <- retryOn null $
    SI.getProjectCommitBuildIds environment p c
  pure $ res === [i]

prop_projectCommitSeen p c b = once . testAWS . withClean environment (SI.deleteProjectCommit environment p c) $ do
  SI.addProjectCommitSeen environment p c b
  res <- retryOn null $
    SI.getProjectCommitSeen environment p c
  pure $ res === [b]

prop_projectCommitSeenDuplicate p c b = once . testAWS . withClean environment (SI.deleteProjectCommit environment p c) $ do
  SI.addProjectCommitSeen environment p c b
  res1 <- retryOn null $
    SI.getProjectCommitSeen environment p c
  SI.addProjectCommitSeen environment p c b
  res2 <- retryOn null $
    SI.getProjectCommitSeen environment p c
  pure $ [res1, res2] === [[b], [b]]

prop_projectCommitDiscovered p c b = once . testAWS . withClean environment (SI.deleteProjectCommit environment p c) $ do
  SI.addProjectCommitDiscovered environment p c b
  res <- retryOn null $
    SI.getProjectCommitDiscovered environment p c
  pure $ res === [b]

prop_projectCommitDiscoveredDuplicate p c b = once . testAWS . withClean environment (SI.deleteProjectCommit environment p c) $ do
  SI.addProjectCommitDiscovered environment p c b
  res1 <- retryOn null $
    SI.getProjectCommitDiscovered environment p c
  SI.addProjectCommitDiscovered environment p c b
  res2 <- retryOn null $
    SI.getProjectCommitDiscovered environment p c
  pure $ [res1, res2] === [[b], [b]]

prop_buildId p b r i = once . testAWS . withClean environment (SI.deleteBuildIds environment p b r) $ do
  SI.addBuildId environment p b r i
  res <- retryOn null $
    SI.getBuildIds environment p b r
  pure $ res === [i]

prop_queued p b i1 i2 = once . testAWS . withClean environment (SI.deleteQueued environment p b) $ do
  SI.addQueued environment p b i1
  res1 <- retryOn null $
    SI.getQueued environment p b
  SI.addQueued environment p b i2
  res2 <- retryOn ((==) 2 . length) $
    SI.getQueued environment p b
  SI.clearQueued environment p b i2
  res3 <- retryOn null $
    SI.getQueued environment p b
  SI.clearQueued environment p b i1
  res4 <- retryOn (not . null) $
    SI.getQueued environment p b
  pure $ [res1, L.sort res2, res3, res4] === [[i1], L.sort [i1, i2], [i1], []]

prop_buildRef p b r = once . testAWS . withClean environment (SI.deleteBuildRefs environment p b) $ do
  SI.addBuildRef environment p b r
  res <- retryOn null $
    SI.getBuildRefs environment p b
  pure $ res === [r]

prop_buildRef_disabled p b r d0 = once . testAWS . withClean environment (SI.deleteBuildRefs environment p b) $ do
  SI.setBuildDisabled environment p b d0
  d1 <- retryOn (/= d0) $
    SI.isBuildDisabled environment p b
  SI.addBuildRef environment p b r
  d2 <- retryOn id $
    SI.isBuildDisabled environment p b
  pure $ (d1, d2) === (d0, False)

retryOn :: MonadIO m => (b -> Bool) -> m b -> m b
retryOn condition action =
  Retry.retrying policy (const $ pure . condition) (const action)

policy :: Retry.RetryPolicy
policy =
  Retry.constantDelay 200000 <> Retry.limitRetries 5

return []
tests = $quickCheckAll
