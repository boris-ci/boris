{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Boris.Http.Store.Dynamo.Build where

import           Boris.Core.Data
import qualified Boris.Http.Store.Dynamo.Lifecycle as SL
import qualified Boris.Http.Store.Dynamo.Build as SB

import           Control.Monad.IO.Class (MonadIO (..))
import qualified Control.Retry as Retry

import           Disorder.Corpus

import           Jebediah.Data (LogGroup (..), LogStream (..))

import           P

import           Test.Boris.Core.Arbitrary ()
import           Test.IO.Boris.Http.Store.Dynamo
import           Test.QuickCheck
import           Test.Mismi (testAWS)

import           X.Control.Monad.Trans.Either (runEitherT)

prop_store i p b r =
  forAll (elements cooking) $ \l ->
    once . testAWS . withClean environment (SB.delete environment i >> SB.deindex environment p b i) $ do
      void . runEitherT $ SB.register environment p b i
      a <- retryOn (/= Accept) $ SB.acknowledge environment i (LogGroup l) (LogStream l)
      void $ SB.complete environment i r
      pure $ a === Accept

prop_store_heartbeat i p b =
  forAll (elements cooking) $ \l ->
    once . testAWS . withClean environment (SB.delete environment i >> SB.deindex environment p b i) $ do
      void . runEitherT $ SB.register environment p b i
      x0 <- retryOn (/= Accept) $ SB.acknowledge environment i (LogGroup l) (LogStream l)
      x1 <- retryOn (/= BuildNotCancelled) $ SB.heartbeat environment i
      x2 <- retryOn not $ SB.cancel environment i
      x3 <- retryOn (/= BuildCancelled) $ SB.heartbeat environment i
      pure $ (x0, x1, x2, x3) === (Accept, BuildNotCancelled, True, BuildCancelled)

ignore_prop_delete_all_the_things_use_only_if_non_migratable_change_happens_in_dev = once .  testAWS $ do
  SL.destroy environment
  pure $ True

retryOn :: MonadIO m => (b -> Bool) -> m b -> m b
retryOn condition action =
  Retry.retrying policy (const $ pure . condition) (const action)

policy :: Retry.RetryPolicy
policy =
  Retry.constantDelay 200000 <> Retry.limitRetries 5

return []
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 5 })
