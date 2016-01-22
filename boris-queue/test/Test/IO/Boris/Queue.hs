{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Boris.Queue where

import           Boris.Queue (BuildQueue (..))
import qualified Boris.Queue as Q

import           Disorder.Core (failWith)

import           Mismi.SQS (unQueueName)

import           P

import           Test.Boris.Core.Arbitrary ()
import           Test.Boris.Queue.Arbitrary ()
import           Test.QuickCheck
import           Test.Mismi (testAWS)
import           Test.Mismi.Arbitrary ()
import           Test.Mismi.SQS (withQueue)

import           X.Control.Monad.Trans.Either (runEitherT)

prop_queue qn r =
  testAWS . withQueue qn $ \_ ->  do
    let bq = BuildQueue $ unQueueName qn
    Q.put bq r
    candidate <- runEitherT $ Q.get bq
    pure $ case candidate of
      Left e ->
        failWith . Q.renderQueueError $ e
      Right rr ->
        rr === Just r

return []
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
