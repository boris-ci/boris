{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Boris.Http.Store.Dynamo.Tick where

import qualified Boris.Http.Store.Dynamo.Tick as ST

import qualified Data.Text as Text

import           Disorder.Core

import           P

import           Test.Boris.Core.Arbitrary ()
import           Test.IO.Boris.Http.Store.Dynamo
import           Test.QuickCheck
import           Test.Mismi (testAWS)

import           X.Control.Monad.Trans.Either (eitherT)

prop_next =
  once . testAWS . withClean environment (pure ()) .
  eitherT (pure . failWith . Text.pack . show) pure $ do
    x <- ST.next environment
    y <- ST.next environment
    pure . property $ y /= x

return []
tests = $quickCheckAll
