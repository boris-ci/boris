{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Boris.Http.Store.Dynamo.Results where

import           Boris.Core.Data
import           Boris.Http.Store.Dynamo.Results

import qualified Data.List as L
import qualified Data.Set as Set

import           P

import           Test.Boris.Core.Arbitrary ()
import           Test.QuickCheck (Gen, quickCheckAll, (===), forAll, listOf, arbitrary)

master :: Maybe Ref
master =
  Just $ Ref "refs/heads/master"

prop_subset =
  forAll (listOf genResult) $ \input ->
  let
    output = calculateDrops input
  in
    Set.fromList output `Set.isSubsetOf` Set.fromList input

prop_ok =
  forAll (listOf genResultOk) $ \input ->
    let
      real = ordNub input
    in
      L.sort (calculateDrops real) === L.sort real

prop_ko =
  forAll genResultKo $ \input ->
    calculateDrops [input] === []

prop_latest_build_id p b =
  let
    br = BuildKo
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
    nr = Just $ Ref "refs/heads/topic/foo"
    input = [
        Result (BuildId "10") p b nr br
      , Result (BuildId "9") p b master BuildKo
      ]
    expected = [
        Result (BuildId "10") p b nr br
      ]
  in
    calculateDrops input === expected

prop_ref_no_master p b br =
  let
    nr = Just $ Ref "refs/heads/topic/foo"
    input = [
        Result (BuildId "10") p b nr br
      ]
    expected = [
        Result (BuildId "10") p b nr br
      ]
  in
    calculateDrops input === expected

genResult :: Gen Result
genResult =
  Result
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

genResultOk :: Gen Result
genResultOk =
  Result
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure BuildOk

genResultKo :: Gen Result
genResultKo =
  Result
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure master
    <*> pure BuildKo

return []
tests = $quickCheckAll
