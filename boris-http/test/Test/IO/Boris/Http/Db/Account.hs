{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Boris.Http.Db.Account where

import           Boris.Http.Data
import qualified Boris.Http.Db.Account as AccountDb

import           Hedgehog

import           P

import           System.IO (IO)

import qualified Test.Boris.Http.Gen as Gen
import           Test.IO.Boris.Http.Db.Test

prop_user :: Property
prop_user =
  property $ do
    github1 <- forAll Gen.genGithubUser
    github2 <- forAll Gen.genGithubUser
    actual <- db $ do
      user1 <- AccountDb.addUser github1
      muser1 <- AccountDb.userByGithubId (githubUserId github1)
      AccountDb.updateUser (user1 { userOf = github2 })
      muser2 <- AccountDb.userByGithubId (githubUserId github2)
      pure (userOf user1, userOf <$> muser1, userOf <$> muser2)
    actual === (github1, Just github1, Just github2)


tests :: IO Bool
tests =
  checkDb $$(discover)
