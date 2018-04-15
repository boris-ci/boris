{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Boris.Http.Db.Schema where

import           Control.Monad.Morph (hoist, lift)

import qualified Boris.Http.Db.Schema as Schema

import           Hedgehog

import           P

import           System.IO (IO)

import           Test.IO.Boris.Http.Db.Test

prop_schema :: Property
prop_schema =
  property $ do
    pool <- mkPool
    evalExceptT . hoist lift $ Schema.initialise pool

tests :: IO Bool
tests =
  checkDb $$(discover)
