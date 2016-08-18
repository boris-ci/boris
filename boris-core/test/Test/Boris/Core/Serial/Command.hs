{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Boris.Core.Serial.Command where

import           Boris.Core.Data
import           Boris.Core.Serial.Command

import qualified Data.Text.IO as T

import           Disorder.Core.IO

import           P

import           Test.QuickCheck



prop_parse_ok =
  conjoin [
      check "test/data/config/command/v1/empty.toml" . Right $ [
        ]
    , check "test/data/config/command/v1/basic.toml" . Right $ [
          Specification
            (Build "basic")
            [Command "tsar" ["pre"]]
            [Command "./mafia" ["build"]]
            []
            [Command "tsar" ["success"]]
            [Command "tsar" ["failure"]]
        ]
    , check "test/data/config/command/v1/inferred.toml" . Right $ [
          Specification
            (Build "basic")
            [Command "tsar" ["pre"]]
            [Command "./mafia" ["build"]]
            []
            [Command "tsar" ["success"]]
            [Command "tsar" ["failure"]]
        , Specification
            (Build "inferred")
            [Command "tsar" ["pre"]]
            [Command "master" ["build", "inferred"]]
            []
            [Command "tsar" ["success"]]
            [Command "tsar" ["failure"]]
        ]
    , check "test/data/config/command/v1/multiple.toml" . Right $ [
          Specification
            (Build "basic")
            [Command "tsar" ["pre"]]
            [Command "master" ["build", "dist"]]
            []
            [Command "tsar" ["success"]]
            [Command "tsar" ["failure"]]
        , Specification
            (Build "second")
            [Command "tsar" ["pre"]]
            [Command "master" ["build", "second"]]
            []
            [Command "tsar" ["success"]]
            [Command "tsar" ["failure"]]
        , Specification
            (Build "third")
            [Command "tsar" ["pre"]]
            [Command "master" ["build", "third"]]
            []
            [Command "tsar" ["success"]]
            [Command "tsar" ["failure"]]
        ]
    , check "test/data/config/command/v1/all.toml" . Right $ [
          Specification
            (Build "all")
            [Command "before" []]
            [Command "master" ["build", "dist"]]
            [Command "after" []]
            [Command "after-on-success" []]
            [Command "after-on-failure" []]
        ]
    ]

prop_parse_error =
  conjoin [
      check "test/data/config/command/v1/invalid.no-version.toml" . Left $
        ConfigMissingVersionError
    , check "test/data/config/command/v1/invalid.unknown-version.toml" . Left $
        ConfigUnknownVersionError 2
    , check "test/data/config/command/v1/invalid.empty-command.toml" . Left $
        ConfigInvalidCommand (Build "invalid")
    , check "test/data/config/command/v1/invalid.build-name.toml" . Left $
        ConfigInvalidName "no/slash"
    ]

check path expected =
  testIO $ do
    f <- T.readFile path
    pure $ (fmap (sortOn specificationBuild) $ parseConfig f) === fmap (sortOn specificationBuild) expected


return []
tests = $quickCheckAll
