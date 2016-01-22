{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Boris.Store.Build where

import           Boris.Core.Data
import qualified Boris.Store.Lifecycle as SL
import qualified Boris.Store.Build as SB

import           Mismi (AWS, awsBracket)

import           P

import           Test.Boris.Core.Arbitrary ()
import           Test.IO.Boris.Store
import           Test.QuickCheck
import           Test.Mismi (testAWS)

import           X.Control.Monad.Trans.Either (runEitherT)

prop_store p b i r = once . testAWS . withBuild environment p b i $ do
  void . runEitherT $ SB.register environment p b i
  a <- SB.acknowledge environment p b i
  SB.complete environment p b i r
  pure $ a == Accept

withBuild :: Environment -> Project -> Build -> BuildId -> AWS a -> AWS a
withBuild e p b i f =
  awsBracket (SL.initialise e) (const $ SB.delete e p b i) (const f)

return []
tests = $quickCheckAll
