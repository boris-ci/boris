{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Boris.Core.Serial.Ref where

import           Boris.Core.Data
import           Boris.Core.Serial.Ref

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Disorder.Core.IO

import           P
import qualified Prelude as Unsafe (error)

import           Test.QuickCheck


prop_parse_ok =
  conjoin [
      check "test/data/config/ref/v1/empty.toml" . Right $ [
        ]
    , check "test/data/config/ref/v1/basic.toml" . Right $ [
          BuildPattern
            (newBuildNamePattern "basic")
            (Pattern "refs/heads/basic")
        ]
    , check "test/data/config/ref/v1/multiple.toml" . Right $ [
          BuildPattern
            (newBuildNamePattern "basic")
            (Pattern "refs/heads/basic")
        , BuildPattern
            (newBuildNamePattern "second")
            (Pattern "refs/heads/*")
        ]
    ]

prop_parse_error =
  conjoin [
      check "test/data/config/ref/v1/invalid.no-version.toml" . Left $
        PatternConfigMissingVersionError
    , check "test/data/config/ref/v1/invalid.unknown-version.toml" . Left $
        PatternConfigUnknownVersionError 2
    , check "test/data/config/ref/v1/invalid.no-reference.toml" . Left $
        PatternConfigNoReference (newBuildNamePattern "basic")
    ]

check path expected =
  testIO $ do
    f <- T.readFile path
    let
      sort' = sortOn (renderBuildNamePattern . buildNamePattern)
    pure $ (fmap sort' $ parsePatternConfig f) === fmap sort' expected


-- FIX QQ
newBuildNamePattern :: Text -> BuildNamePattern
newBuildNamePattern t =
  either (Unsafe.error . T.unpack) id . parseBuildNamePattern $ t


return []
tests = $quickCheckAll
