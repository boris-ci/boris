{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Serial.Decode (
    errored
  , forbidden
  , parse
  ) where

import           Boris.Client.Error

import           Data.Aeson (Value, (.:), (.:?))
import           Data.Aeson.Types (Parser)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString.Lazy as Lazy
import           Data.Text (Text)
import qualified Data.Text as Text


errored :: Value -> Parser BorisError
errored =
  Aeson.withObject "BorisError" $ \o ->
    BorisApplicationError
      <$> (ErrorCode <$> o .: "error")
      <*> (fmap ErrorMessage <$> o .:? "message")

forbidden :: Value -> Parser BorisError
forbidden =
  Aeson.withObject "BorisError" $ \o ->
    BorisAuthorizationError
      <$> (ErrorCode <$> o .: "error")
      <*> (fmap ErrorMessage <$> o .:? "message")

parse :: (Value -> Parser a) -> Lazy.ByteString -> Either Text a
parse to t =
  first Text.pack (Aeson.eitherDecode t) >>= \v -> case Aeson.parse to v of
    Aeson.Success a ->
      pure a
    Aeson.Error msg ->
      Left . Text.pack $ msg
