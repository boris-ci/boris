{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Boris.Http.Db.Session  where

import           Boris.Http.Data
import qualified Boris.Http.Db.Account as AccountDb
import qualified Boris.Http.Db.Session as SessionDb

import           Control.Monad.IO.Class (MonadIO (..))

import           Hedgehog

import           P

import           System.IO (IO)

import qualified Test.Boris.Http.Gen as Gen
import           Test.IO.Boris.Http.Db.Test

prop_session :: Property
prop_session =
  property $ do
    sessionId <- liftIO newSessionToken
    github <- forAll Gen.genGithubUser
    oauth <- forAll Gen.genGithubOAuth
    let session = Session sessionId oauth
    (user, actual) <- db $ do
      user <- AccountDb.addUser github
      SessionDb.newSession session (userIdOf user)
      SessionDb.tickSession sessionId
      result <- SessionDb.getSession sessionId
      pure (user, result)
    actual === (Just $ AuthenticatedUser user $ Session sessionId oauth)

tests :: IO Bool
tests =
  checkDb $$(discover)
