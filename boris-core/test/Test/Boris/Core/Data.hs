{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Boris.Core.Data where

import           Boris.Core.Data

import           Data.List (sort)
import qualified Data.Text as T

import           P

import           Test.QuickCheck


prop_build_id_sort (ids :: [Int]) =
  to (reverse $ sort ids) === sortBuildIds (to ids)
  where
    to = fmap (BuildId . T.pack . show)


return []
tests = $quickCheckAll
