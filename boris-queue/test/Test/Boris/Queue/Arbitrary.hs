{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Boris.Queue.Arbitrary where

import           Boris.Core.Data
import           Boris.Queue

import           P

import           Test.Boris.Core.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


instance Arbitrary RequestBuild where
  arbitrary =
    RequestBuild
      <$> arbitrary
      <*> arbitrary
      <*> (Repository <$> arbitrary)
      <*> arbitrary
      <*> oneof [fmap (Just . Ref) arbitrary, pure Nothing]

instance Arbitrary RequestDiscover where
  arbitrary =
    RequestDiscover
      <$> arbitrary
      <*> arbitrary
      <*> (Repository <$> arbitrary)

instance Arbitrary Request where
  arbitrary =
    oneof [
        RequestDiscover' <$> arbitrary
      , RequestBuild' <$> arbitrary
      ]
