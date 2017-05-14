{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Boris.Store.Tick where

import qualified Boris.Store.Tick as ST

import           Disorder.Core

import           P

import           Test.Boris.Core.Arbitrary ()
import           Test.IO.Boris.Store
import           Test.QuickCheck
import           Test.Mismi (testAWS)

import           X.Control.Monad.Trans.Either (eitherT)

prop_next p b =
  once . testAWS . withClean environment (pure ()) .
  eitherT (pure . failWith . ST.renderTickError) pure $ do
    x <- ST.next environment p b
    y <- ST.next environment p b
    pure . property $ y /= x

return []
tests = $quickCheckAll
