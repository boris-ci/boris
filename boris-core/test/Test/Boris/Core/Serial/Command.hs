{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Boris.Core.Serial.Command where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Configuration
import           Boris.Core.Serial.Command
import           Boris.Prelude

import           Control.Monad.IO.Class (MonadIO (..))

import qualified Data.List as List
import qualified Data.Text.IO as Text

import           Hedgehog hiding (Command)

import           System.IO (IO, FilePath)


checker :: FilePath -> Either BorisConfigError [Specification] -> PropertyT IO ()
checker path expected = do
  f <- liftIO $ Text.readFile path
  let r = parseConfig f
  annotateShow expected
  annotateShow r
  fmap (List.sortOn specificationBuild) r === fmap (List.sortOn specificationBuild) expected

prop_parse_ok :: Property
prop_parse_ok =
  property $ do
    checker "test/data/config/command/v1/empty.toml" . Right $ [
      ]
    checker "test/data/config/command/v1/basic.toml" . Right $ [
        Specification
          (Build "basic")
          []
          [Command "./mafia" ["build"]]
          []
          []
          []
      ]
    checker "test/data/config/command/v1/inferred.toml" . Right $ [
        Specification
          (Build "basic")
          []
          [Command "./mafia" ["build"]]
          []
          []
          []
      , Specification
          (Build "inferred")
          []
          []
          []
          []
          []
      ]
    checker "test/data/config/command/v1/multiple.toml" . Right $ [
        Specification
          (Build "basic")
          []
          [Command "master" ["build", "dist"]]
          []
          []
          []
      , Specification
          (Build "second")
          []
          [Command "master" ["build", "second"]]
          []
          []
          []
      , Specification
          (Build "third")
          []
          []
          []
          []
          []
      ]
    checker "test/data/config/command/v1/all.toml" . Right $ [
        Specification
          (Build "all")
          [Command "before" []]
          [Command "master" ["build", "dist"]]
          [Command "after" []]
          [Command "after-on-success" []]
          [Command "after-on-failure" []]
      ]

prop_parse_error :: Property
prop_parse_error =
  property $ do
    checker "test/data/config/command/v1/invalid.no-version.toml" . Left $
      ConfigMissingVersionError
    checker "test/data/config/command/v1/invalid.unknown-version.toml" . Left $
      ConfigUnknownVersionError 2
    checker "test/data/config/command/v1/invalid.empty-command.toml" . Left $
      ConfigInvalidCommand (Build "invalid")
    checker "test/data/config/command/v1/invalid.build-name.toml" . Left $
      ConfigInvalidName "no/slash"

tests :: IO Bool
tests =
  checkParallel $$(discover)
