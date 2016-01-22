{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Boris.Core.Serial.Toml where

import           Boris.Core.Data
import           Boris.Core.Serial.Toml

import qualified Data.Text.IO as T

import           Disorder.Core.IO

import           P

import           Test.QuickCheck



prop_parse_ok =
  conjoin [
      check "test/data/config/v1/empty.toml" . Right $ [
        ]
    , check "test/data/config/v1/basic.toml" . Right $ [
          Specification
            (Build "basic")
            (Query "refs/heads/basic")
            []
            [Command "master" ["build", "basic"]]
            []
            []
            []
        ]
    , check "test/data/config/v1/multiple.toml" . Right $ [
          Specification
            (Build "basic")
            (Query "refs/heads/basic")
            []
            [Command "master" ["build", "basic"]]
            []
            []
            []
        , Specification
            (Build "second")
            (Query "refs/heads/*")
            []
            [Command "master" ["build", "second"]]
            []
            []
            []
        ]
    , check "test/data/config/v1/command.toml" . Right $ [
          Specification
            (Build "basic")
            (Query "refs/heads/basic")
            []
            [Command "master" ["build", "basic"]]
            []
            []
            []
        , Specification
            (Build "second")
            (Query "refs/heads/second")
            []
            [Command "master" ["build", "second"]]
            []
            []
            []
        , Specification
            (Build "command")
            (Query "refs/heads/command")
            []
            [Command "./mafia" ["test"]]
            []
            []
            []
        ]
    ]

prop_parse_error =
  conjoin [
      check "test/data/config/v1/invalid.no-version.toml" . Left $
        ConfigMissingVersionError
    , check "test/data/config/v1/invalid.unknown-version.toml" . Left $
        ConfigUnknownVersionError 2
    , check "test/data/config/v1/invalid.no-reference.toml" . Left $
        ConfigNoReference (Build "basic")
    , check "test/data/config/v1/invalid.empty-command.toml" . Left $
        ConfigInvalidCommand (Build "command")
    ]

check path expected =
  testIO $ do
    f <- T.readFile path
    pure $ (fmap (sortOn specificationBuild) $ parseConfig f) === fmap (sortOn specificationBuild) expected


return []
tests = $quickCheckAll
