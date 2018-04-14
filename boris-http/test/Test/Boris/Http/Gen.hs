{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Boris.Http.Gen where

import           Boris.Http.Data

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           P


genGithubClient :: Gen GithubClient
genGithubClient =
  fmap (GithubClient . Text.pack . show) $
    Gen.int (Range.constant 0 99999)

genGithubSecret :: Gen GithubSecret
genGithubSecret =
  fmap (GithubSecret . Text.pack . show) $
    Gen.int (Range.constant 0 99999)

genGithubUser :: Gen GithubUser
genGithubUser =
  GithubUser
    <$> genGithubId
    <*> genGithubLogin
    <*> Gen.maybe genGithubName
    <*> Gen.maybe genGithubEmail

genGithubLogin :: Gen GithubLogin
genGithubLogin =
  GithubLogin <$> Gen.element [
      "kyle"
    , "stan"
    , "kenny"
    , "cartman"
    ]

genGithubName :: Gen GithubName
genGithubName =
  GithubName <$> Gen.element [
      "Kyle"
    , "Stan"
    , "Kenny"
    , "Cartman"
    ]

genGithubEmail :: Gen GithubEmail
genGithubEmail =
  GithubEmail <$> Gen.element [
      "kyle@southpark.tv"
    , "stan@southpark.tv"
    , "kenny@southpark.tv"
    , "cartman@southpark.tv"
    ]

genGithubCode :: Gen GithubCode
genGithubCode =
  fmap (GithubCode . Text.pack . show) $
    Gen.int (Range.constant 0 99999)

genGithubOAuth :: Gen GithubOAuth
genGithubOAuth =
  fmap (GithubOAuth . Text.encodeUtf8 . Text.pack . show) $
    Gen.int (Range.constant 0 99999)

genGithubId :: Gen GithubId
genGithubId =
  fmap GithubId $
    Gen.int (Range.constant 0 99999)
