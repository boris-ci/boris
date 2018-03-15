{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Log (
    LogError
  , renderLogError
  , withLogger
  ) where

import           Control.Monad.IO.Class (MonadIO (..))

import           Boris.Service.Boot

import qualified Data.Conduit.Binary as CB

import           Mismi (Error, renderError)

import           P

import           System.IO (IO, stdout)

import           Tine.Conduit (Out)

import           X.Control.Monad.Trans.Either (EitherT)


data LogError =
  LogAwsError Error

renderLogError :: LogError -> Text
renderLogError err =
  case err of
    LogAwsError e ->
      mconcat ["Underlying AWS error while logging: ", renderError e]

withLogger :: LogService -> (Out -> IO a) -> EitherT LogError IO a
withLogger service f =
  case service of
    Std ->
      liftIO $ f (CB.sinkHandle stdout)
