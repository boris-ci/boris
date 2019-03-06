{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Boris.Service.Git where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Configuration
import           Boris.Service.Git (InitialiseError (..), findRef)
import           Boris.Prelude

import qualified Data.Text as T

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Test.Boris.Core.Gen
import           System.IO (IO)



prop_findRef_none :: Property
prop_findRef_none =
  property $ do
    b <- forAll genBuildName
    p <- forAll genBuildPattern
    x <- forAll $ Gen.maybe genRef
    findRef b p x [] === Left (NoMatchingRef b p)

prop_findRef_exact :: Property
prop_findRef_exact =
  property $ do
    b <- forAll genBuildName
    p <- forAll genBuildPattern
    r <- forAll genRef
    findRef b p Nothing [r] === Right r

prop_findRef_exact_with_target :: Property
prop_findRef_exact_with_target =
  property $ do
    b <- forAll genBuildName
    p <- forAll genBuildPattern
    r <- forAll genRef
    findRef b p (Just r) [r] === Right r

prop_findRef_exact_with_target_mismatch :: Property
prop_findRef_exact_with_target_mismatch =
  property $ do
    b <- forAll genBuildName
    p <- forAll genBuildPattern
    r <- forAll genRef
    x <- forAll $ Gen.filter (/= r) genRef
    findRef b p (Just x) [r] === Left (MismatchedRef b p x [r])

prop_findRef_multi_without_target :: Property
prop_findRef_multi_without_target =
  property $ do
    b <- forAll genBuildName
    p <- forAll genBuildPattern
    r <- forAll genRef
    x <- forAll $ Gen.filter (/= r) genRef
    findRef b p Nothing [r, x] === Left (AmbiguousRef b p [r, x])

prop_findRef_multi_with_target :: Property
prop_findRef_multi_with_target =
  property $ do
    b <- forAll genBuildName
    p <- forAll genBuildPattern
    r <- forAll genRef
    x <- forAll $ Gen.filter (/= r) genRef
    findRef b p (Just r) [r, x] === Right r

prop_findRef_multi_with_target_mismatch :: Property
prop_findRef_multi_with_target_mismatch =
  property $ do
    b <- forAll genBuildName
    p <- forAll genBuildPattern
    r <- forAll genRef
    x <- forAll $ Gen.filter (/= r) genRef
    y <- forAll . Gen.filter (/= x) . Gen.filter (/= r) $ genRef
    findRef b p (Just y) [r, x] === Left (MismatchedRef b p y [r, x])

prop_matchesBuild :: Property
prop_matchesBuild =
  property $ do
    BuildWithPattern b g <- forAll genBuildWithPattern
    assert $ matchesBuild g b

prop_matchesBuild_examples :: Property
prop_matchesBuild_examples =
  property $ do
    let
      match p b =
        case parseBuildNamePattern p of
          Left message -> do
            annotate . T.unpack $ message
            failure
          Right v ->
            if matchesBuild v (BuildName b) then
              success
            else do
              annotate . T.unpack $ p <> " can't match " <> b
              failure

      nomatch p b =
        case parseBuildNamePattern p of
          Left message -> do
            annotate . T.unpack $ message
            failure
          Right v ->
            if matchesBuild v (BuildName b) then do
              annotate . T.unpack $ p <> " shouldn't match " <> b
              failure
            else
              success

    match "a" "a"
    match "a*" "a"
    match "a*" "abc"
    match "*c" "abc"
    match "*c*" "abcde"
    -- Single character matches
    match "a?" "ab"
    nomatch "a?" "abc"
    -- We've _only_ enabled * and ?
    nomatch "[abc]" "a"
    match "[abc]" "[abc]"
    -- NOTE: This is really a glob, and slashes _do_ mean something
    match "a*/c" "ab/c"
    nomatch "a*" "a/b"
    -- No recursive wildcards
    nomatch "a/**" "a/b/c/d"


tests :: IO Bool
tests =
  checkParallel $$(discover)
