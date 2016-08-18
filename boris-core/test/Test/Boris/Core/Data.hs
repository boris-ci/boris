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

import           Test.Boris.Core.Arbitrary ()
import           Test.QuickCheck


prop_build_id_sort (ids :: [Int]) =
  to (reverse $ sort ids) === sortBuildIds (to ids)
  where
    to = fmap (BuildId . T.pack . show)

prop_build_create =
  isJust . newBuild . renderBuild

prop_build_create_invalid =
  once $ conjoin [
      newBuild "no/slash" === Nothing
    , newBuild "/noslash" === Nothing
    , newBuild "noslash/" === Nothing
    ]

return []
tests = $quickCheckAll
