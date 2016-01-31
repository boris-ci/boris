{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Boris.Store.Build where

import           Boris.Core.Data
import qualified Boris.Store.Lifecycle as SL
import qualified Boris.Store.Build as SB

import           Disorder.Corpus

import           Jebediah.Data (GroupName (..), StreamName (..))

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
      a <- SB.acknowledge environment i (GroupName l) (StreamName l)
      SB.complete environment i r
      pure $ a == Accept

ignore_prop_delete_all_the_things_use_only_if_non_migratable_change_happens_in_dev = once .  testAWS $ do
  SL.destroy environment
  pure $ True

return []
tests = $quickCheckAll
