{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Boris.Store.Build where

import           Boris.Core.Data
import qualified Boris.Store.Lifecycle as SL
import           Boris.Store.Build (BuildCancelled (..))
import qualified Boris.Store.Build as SB

import           Disorder.Corpus

import           Jebediah.Data (LogGroup (..), LogStream (..))

import           P

import           Test.Boris.Core.Arbitrary ()
import           Test.IO.Boris.Store
import           Test.QuickCheck
import           Test.Mismi (testAWS)

import           X.Control.Monad.Trans.Either (runEitherT)

prop_store i p b r =
  forAll (elements cooking) $ \l ->
    once . testAWS . withClean environment (SB.delete environment i >> SB.deindex environment p b i) $ do
      void . runEitherT $ SB.register environment p b i
      a <- SB.acknowledge environment i (LogGroup l) (LogStream l)
      void $ SB.complete environment i r
      pure $ a === Accept

prop_store_heartbeat i p b =
  forAll (elements cooking) $ \l ->
    once . testAWS . withClean environment (SB.delete environment i >> SB.deindex environment p b i) $ do
      void . runEitherT $ SB.register environment p b i
      x0 <- SB.acknowledge environment i (LogGroup l) (LogStream l)
      x1 <- SB.heartbeat environment i
      x2 <- SB.cancel environment i
      x3 <- SB.heartbeat environment i
      pure $ (x0, x1, x2, x3) === (Accept, BuildNotCancelled, True, BuildCancelled)

ignore_prop_delete_all_the_things_use_only_if_non_migratable_change_happens_in_dev = once .  testAWS $ do
  SL.destroy environment
  pure $ True

return []
tests = $quickCheckAll
