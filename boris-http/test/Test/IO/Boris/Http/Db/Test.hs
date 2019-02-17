{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Boris.Http.Db.Test (
    db
  , checkDb
  , mkPool
  ) where

import           Boris.Http.Db.Schema
import           Boris.Prelude

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Morph (hoist, lift)

import qualified Nest

import           Traction.Control

import           Hedgehog

import           System.IO (IO)


mkPool :: MonadIO m => m DbPool
mkPool =
  liftIO $ do
    dbstring <- Nest.force $
      Nest.string "BORIS_POSTGRES"
        `Nest.withDefault`
          "dbname=boris_test host=localhost user=boris_test password=boris_test port=5432"
    newPool dbstring

db :: Db a -> PropertyT IO a
db x = do
  pool <- mkPool
  evalExceptT . hoist lift . testDb pool $ migrate >> x

checkDb :: MonadIO m => Group -> m Bool
checkDb group =
  case group of
    Group name properties ->
      checkSequential (Group name ((fmap . fmap) (withTests 5) properties))
