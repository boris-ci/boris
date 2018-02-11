{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Boris.Core.Data where

import           Boris.Core.Data


import           Data.List (sort)
import qualified Data.Text as T

import           Disorder.Core ((=/=))

import           P

import           Test.Boris.Core.Arbitrary ()
import           Test.QuickCheck


prop_build_id_sort (ids :: [Int]) =
  to (reverse $ sort ids) === sortBuildIds (to ids)
  where
    to = fmap (BuildId . T.pack . show)

prop_build_id_ord =
  ordLaws (arbitrary :: Gen BuildId) compare

prop_build_create =
  isJust . newBuild . renderBuild

prop_build_create_invalid =
  once $ conjoin [
      newBuild "no/slash" === Nothing
    , newBuild "/noslash" === Nothing
    , newBuild "noslash/" === Nothing
    ]

ordLaws :: (Eq a, Show a) => Gen a -> (a -> a -> Ordering) -> Property
ordLaws genA f =
  forAll genA $ \c1 ->
  forAll genA $ \c2 ->
  forAll genA $ \c3 ->
    conjoin [
        f c1 c1 === EQ
      , if c1 /= c2 then f c1 c2 =/= EQ else property True
      , f c1 c2 === complimentOrd (f c2 c1)
      , if f c1 c2 /= GT && f c2 c3 /= GT then f c1 c3 =/= GT else property True
      ]

complimentOrd :: Ordering -> Ordering
complimentOrd o =
  case o of
    EQ ->
      EQ
    LT ->
      GT
    GT ->
      LT

return []
tests = $quickCheckAll
