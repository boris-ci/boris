{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Boris.Core.Arbitrary where

import qualified Data.Text as T

import           Disorder.Corpus

import           Boris.Core.Data

import           P

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

instance Arbitrary Project where
  arbitrary =
    Project <$> elements muppets

instance Arbitrary Build where
  arbitrary =
    Build <$> elements simpsons

instance Arbitrary BuildId where
  arbitrary =
    (BuildId . T.pack . show) <$> choose (1 :: Int, 10000)

instance Arbitrary BuildResult where
  arbitrary =
    elements [BuildOk, BuildKo]
