{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Boris.Http.Db.Schema where

import           Control.Monad.Morph (hoist, lift)

import qualified Boris.Http.Store.Api as Store
import           Boris.Http.Store.Data

import           Hedgehog

import           P

import           System.IO (IO)

import           Test.IO.Boris.Http.Store.Postgres.Test

prop_schema :: Property
prop_schema =
  property $ do
    pool <- mkPool
    evalExceptT . hoist lift $ Store.initialise (PostgresStore pool)

tests :: IO Bool
tests =
  checkDb $$(discover)
