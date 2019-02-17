{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Git.Pin (
    Pin
  , newPin
  , checkPin
  , waitForPin
  , pullPin
  ) where

import           Boris.Prelude

import           Control.Concurrent.MVar

import           System.IO

newtype Pin =
  Pin { pin :: MVar () }

newPin :: IO Pin
newPin =
  Pin <$> newEmptyMVar

checkPin :: Pin -> IO Bool
checkPin =
  fmap isJust . tryTakeMVar . pin

waitForPin :: Pin -> IO ()
waitForPin =
  void . readMVar . pin

pullPin :: Pin -> IO ()
pullPin =
  void . flip tryPutMVar () . pin
