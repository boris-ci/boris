{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Http.Form (
    loadForm
  ) where

import           Airship (Webmachine, parseFormData, request)

import           Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.Text.Encoding as T

import           P

loadForm :: MonadIO m => Webmachine m [(Text, Text)]
loadForm =
  request
    >>= liftIO . parseFormData
    >>= return . fmap (\(k, v) -> (T.decodeUtf8 k, T.decodeUtf8 v)) . fst
