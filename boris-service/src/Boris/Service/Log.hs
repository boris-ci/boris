{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Log (
    LogError
  , renderLogError
  , withLogger
  ) where

import           Control.Monad.IO.Class (MonadIO (..))

import           Boris.Git.X (Out)
import           Boris.Service.Boot
import           Boris.Prelude

import qualified Data.Conduit.Binary as CB

import           System.IO (IO, stdout)

data LogError =
  LogError

renderLogError :: LogError -> Text
renderLogError err =
  case err of
    LogError ->
      mconcat ["Underlying error while logging TODO."]

withLogger :: LogService -> (Out -> IO a) -> EitherT LogError IO a
withLogger service f =
  case service of
    Std ->
      liftIO $ f (CB.sinkHandle stdout)
