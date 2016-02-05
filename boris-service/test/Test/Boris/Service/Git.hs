{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Boris.Service.Git where

import           Boris.Service.Git (InitialiseError (..), findRef)

import           P

import           Test.Boris.Core.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_findRef_none b p x =
  findRef b p x [] === Left (NoMatchingRef b p)

prop_findRef_exact b p r =
  findRef b p Nothing [r] === Right r

prop_findRef_exact_with_target b p r =
  findRef b p (Just r) [r] === Right r

prop_findRef_exact_with_target_mismatch b p r x = r /= x ==>
  findRef b p (Just x) [r] === Left (MismatchedRef b p x [r])

prop_findRef_multi_without_target b p r x = r /= x ==>
  findRef b p Nothing [r, x] === Left (AmbiguousRef b p [r, x])

prop_findRef_multi_with_target b p r x = r /= x ==>
  findRef b p (Just r) [r, x] === Right r

prop_findRef_multi_with_target_mismatch b p r x y = r /= x && r /= y && x /= y ==>
  findRef b p (Just y) [r, x] === Left (MismatchedRef b p y [r, x])

return []
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 })
