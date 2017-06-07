{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Boris.Store.Results where

import           Boris.Core.Data
import           Boris.Store.Results

import           P

import           Test.Boris.Core.Arbitrary ()
import           Test.QuickCheck (quickCheckAll, (===))

master :: Ref
master =
  Ref "refs/heads/master"

prop_latest_build_id p b br =
  let
    input = [
        Result (BuildId "10") p b master br
      , Result (BuildId "9") p b master br
      , Result (BuildId "8") p b master br
      , Result (BuildId "7") p b master br
      , Result (BuildId "6") p b master br
      ]
    expected = [
        Result (BuildId "6") p b master br
      , Result (BuildId "7") p b master br
      , Result (BuildId "8") p b master br
      , Result (BuildId "9") p b master br
      ]
  in
    calculateDrops input === expected

prop_ref_master p b br =
  let
    nr = Ref "refs/heads/topic/foo"
    input = [
        Result (BuildId "10") p b nr br
      , Result (BuildId "9") p b master br
      ]
    expected = [
        Result (BuildId "10") p b nr br
      ]
  in
    calculateDrops input === expected

prop_ref_no_master p b br =
  let
    nr = Ref "refs/heads/topic/foo"
    input = [
        Result (BuildId "10") p b nr br
      ]
    expected = [
        Result (BuildId "10") p b nr br
      ]
  in
    calculateDrops input === expected

return []
tests = $quickCheckAll
