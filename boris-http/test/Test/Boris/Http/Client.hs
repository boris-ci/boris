{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Boris.Http.Client where

import           Boris.Core.Data
import qualified Boris.Client.Project as Project

import           Hedgehog

import           P

import           Test.Boris.Http.Server

import           System.IO (IO)

prop_get_projects :: Property
prop_get_projects =
  property $ do
    projects <- withServerT $ \config -> do
      Project.list config
    projects === [Project "demo"]

tests :: IO Bool
tests =
  checkParallel $$(discover)
