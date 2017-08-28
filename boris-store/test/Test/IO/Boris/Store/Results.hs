{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Boris.Store.Results where

import           Boris.Core.Data
import           Boris.Store.Results (Result (..))
import qualified Boris.Store.Results as Store

import           Control.Monad.IO.Class (MonadIO (..))
import qualified Control.Retry as Retry

import           P

import           Test.Boris.Core.Arbitrary ()
import           Test.IO.Boris.Store
import           Test.QuickCheck
import           Test.Mismi (testAWS)

import           X.Control.Monad.Trans.Either

prop_add i p b r br =
  once . testAWS . withClean environment (Store.deleteItem environment) $ do
    z <- runEitherT $ Store.add environment (Result i p b r br)
    pure $ z === Right ()

prop_add_fetch i p b r br =
  once . testAWS . withClean environment (Store.deleteItem environment) $ do
    _ <- runEitherT $ Store.add environment (Result i p b r br)
    l <- runEitherT . waitFor (not . null) $
      Store.fetch environment
    pure $ l === Right [Result i p b r br]

prop_add_compress_fetch p b r br =
  once . testAWS . withClean environment (Store.deleteItem environment) $ do
    _ <- runEitherT $ Store.add environment (Result (BuildId "10") p b master BuildKo)
    _ <- runEitherT $ Store.add environment (Result (BuildId "9") p b (Just r) br)
    z <- runEitherT $ Store.addWithCompressLimit environment 2 (Result (BuildId "8") p b (Just r) br)
    l <- runEitherT . waitFor (not . null) $ Store.fetch environment
    pure $ (z, l) === (Right (), Right [Result (BuildId "10") p b master BuildKo])

prop_add_compress i p b r br =
  once . testAWS . withClean environment (Store.deleteItem environment) $ do
    _ <- runEitherT $ Store.add environment (Result i p b master BuildKo)
    _ <- runEitherT $ Store.add environment (Result i p b r br)
    l <- runEitherT . waitFor (not . null) $ Store.compress environment
    pure $ l === Right [Result i p b master BuildKo]

prop_add_compress_no_master i p b br =
  once . testAWS . withClean environment (Store.deleteItem environment) $ do
    _ <- runEitherT $ Store.add environment (Result i p b (Just $ Ref "refs/heads/topic/foo") br)
    l <- runEitherT $ Store.compress environment
    z <- runEitherT $ Store.fetch environment
    pure $ (l, z) === (Right [], Right [])

master :: Maybe Ref
master =
  Just $ Ref "refs/heads/master"

waitFor :: MonadIO m => (b -> Bool) -> m b -> m b
waitFor condition action =
  Retry.retrying policy (const $ pure . condition) (const action)

policy :: Retry.RetryPolicy
policy =
  Retry.constantDelay 200000 <> Retry.limitRetries 5

return []
tests = $quickCheckAll
