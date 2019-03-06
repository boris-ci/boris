{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Boris.Core.Serial.Ref where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Configuration
import           Boris.Core.Serial.Ref
import           Boris.Prelude

import           Control.Monad.IO.Class (MonadIO (..))

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Hedgehog

import qualified Prelude as Unsafe (error)

import           System.IO (IO, FilePath)


checker :: FilePath -> Either BorisPatternConfigError [BuildPattern] -> PropertyT IO ()
checker path expected = do
  f <- liftIO $ Text.readFile path
  let sorter = List.sortOn (renderBuildNamePattern . buildNamePattern)
  (fmap sorter $ parsePatternConfig f) === fmap sorter expected

prop_parse_ok :: Property
prop_parse_ok =
  property $ do
    checker "test/data/config/ref/v1/empty.toml" . Right $ [
      ]
    checker "test/data/config/ref/v1/basic.toml" . Right $ [
        BuildPattern
          (newBuildNamePattern "basic")
          (Pattern "refs/heads/basic")
      ]
    checker "test/data/config/ref/v1/multiple.toml" . Right $ [
        BuildPattern
          (newBuildNamePattern "basic")
          (Pattern "refs/heads/basic")
      , BuildPattern
          (newBuildNamePattern "second")
          (Pattern "refs/heads/*")
      ]

    checker "test/data/config/ref/v1/wildcard.toml" . Right $ [
        BuildPattern
          (newBuildNamePattern "test-*")
          (Pattern "refs/heads/test-*")
      ]

prop_parse_error :: Property
prop_parse_error =
  property $ do
    checker "test/data/config/ref/v1/invalid.no-version.toml" . Left $
      PatternConfigMissingVersionError
    checker "test/data/config/ref/v1/invalid.unknown-version.toml" . Left $
      PatternConfigUnknownVersionError 2
    checker "test/data/config/ref/v1/invalid.no-reference.toml" . Left $
      PatternConfigNoReference (newBuildNamePattern "basic")

-- FIX QQ
newBuildNamePattern :: Text -> BuildNamePattern
newBuildNamePattern t =
  either (Unsafe.error . Text.unpack) id . parseBuildNamePattern $ t

tests :: IO Bool
tests =
  checkParallel $$(discover)
