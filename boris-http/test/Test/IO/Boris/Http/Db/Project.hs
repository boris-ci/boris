{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Boris.Http.Db.Project where

import           Boris.Core.Data.Build
import qualified Boris.Http.Db.Project as ProjectDb

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           P

import           System.IO (IO)

import qualified Test.Boris.Core.Gen as Gen
import           Test.IO.Boris.Http.Db.Test


prop_create :: Property
prop_create =
  property $ do
    t <- forAll Gen.genOwnerType
    owner <- forAll Gen.genOwnerName
    project <- forAll Gen.genProject
    repository <- forAll Gen.genRepository
    p <- db $
      ProjectDb.createProject t owner project repository

    p === p

tests :: IO Bool
tests =
  checkDb $$(discover)
