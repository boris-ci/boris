{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Boris.Queue where

import           Boris.Core.Data
import           Boris.Queue (BuildQueue (..))
import qualified Boris.Queue as Q

import           Disorder.Core (failWith)

import           Mismi.SQS (renderQueueName)

import           P

import           Test.Boris.Core.Arbitrary ()
import           Test.Boris.Queue.Arbitrary ()
import           Test.Mismi (testAWS)
import           Test.Mismi.Arbitrary ()
import           Test.Mismi.SQS (withQueue)
import           Test.QuickCheck

import           X.Control.Monad.Trans.Either (runEitherT)

prop_queue qn r =
  testAWS . withQueue qn $ \_ ->  do
    let bq = BuildQueue $ renderQueueName qn
    Q.put bq r
    candidate <- runEitherT $ Q.get bq
    pure $ case candidate of
      Left e ->
        failWith . Q.renderQueueError $ e
      Right rr ->
        rr === Just r

prop_queue_size qn r =
  testAWS . withQueue qn $ \_ ->  do
    let bq = BuildQueue $ renderQueueName qn
    Q.put bq r
    size <- Q.size bq
    pure $ size === QueueSize 1

return []
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
