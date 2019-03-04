{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Boris.Core.Data.Build where

import           Boris.Core.Data.Build
import           Boris.Prelude

import           Data.List (sort, reverse)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           System.IO (IO)

import           Test.Boris.Core.Gen

prop_build_id_sort :: Property
prop_build_id_sort =
  property $ do
    let
      to = fmap BuildId
    ids <- forAll $ Gen.list (Range.linear 0 100) (Gen.int64 (Range.linear 0 100))
    to (reverse $ sort ids) === sortBuildIds (to ids)

prop_build_id_ord :: Property
prop_build_id_ord =
  ordLaws genBuildId compare

prop_build_create :: Property
prop_build_create =
  property $ do
    b <- forAll genBuildName
    assert . isJust . newBuildName . renderBuildName $ b

prop_build_create_invalid :: Property
prop_build_create_invalid =
  property $ do
    newBuildName "no/slash" === Nothing
    newBuildName "/noslash" === Nothing
    newBuildName "noslash/" === Nothing

ordLaws :: (Eq a, Show a) => Gen a -> (a -> a -> Ordering) -> Property
ordLaws genA f =
  property $ do
    c1 <- forAll genA
    c2 <- forAll genA
    c3 <- forAll genA
    f c1 c1 === EQ
    when (c1 /= c2) $
      assert (not $  f c1 c2 == EQ)
    f c1 c2 === complimentOrd (f c2 c1)
    when (f c1 c2 /= GT && f c2 c3 /= GT) $
      assert (not $ f c1 c3 == GT)

complimentOrd :: Ordering -> Ordering
complimentOrd o =
  case o of
    EQ ->
      EQ
    LT ->
      GT
    GT ->
      LT

tests :: IO Bool
tests =
  checkParallel $$(discover)
