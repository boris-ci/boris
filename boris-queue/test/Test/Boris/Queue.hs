{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Boris.Queue where

import           Boris.Queue

import           Disorder.Core

import           P

import           System.IO

import           Test.Boris.Queue.Arbitrary ()
import           Test.QuickCheck

import           X.Data.Aeson


prop_json_request = tripping' fromRequest toRequest
prop_json_request_v1 = tripping' fromRequestV1 toRequest
prop_json_request_v2 = tripping' fromRequestV2 toRequest

tripping' to fro = tripping (asTextWith to) (asWith fro)

return []
tests :: IO Bool
tests = $quickCheckAll
