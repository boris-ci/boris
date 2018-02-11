{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Boris.Http.Store.Postgres.Test (
    db
  , checkDb
  , mkPool
  ) where

import           Boris.Http.Store.Postgres.Schema

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Morph (hoist, lift)

import           P

import           Traction.Control
import           Traction.Migration

import           Hedgehog

import           System.IO (IO)


mkPool :: MonadIO m => m DbPool
mkPool =
  liftIO $ newPool "dbname=boris_test host=docker.for.mac.localhost user=boris_test password=boris_test port=5432"

db :: Db a -> PropertyT IO a
db x = do
  pool <- mkPool
  evalExceptT . hoist lift . testDb pool $ migrate schema >> x

checkDb :: MonadIO m => Group -> m Bool
checkDb group =
  case group of
    Group name properties ->
      checkSequential (Group name ((fmap . fmap) (withTests 5) properties))
