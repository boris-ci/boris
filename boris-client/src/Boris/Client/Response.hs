{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Client.Response (
    ResponseError (..)

  , Responder (..)

  , json
  , none
  ) where

import           Boris.Prelude
import           Boris.Client.Error
import           Boris.Client.Serial.Decode as Decode


import           Data.Aeson (Value)
import           Data.Aeson.Types (Parser)
import           Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString.Lazy as Lazy
import           Data.Text (Text)

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP


data ResponseError =
    ParseResponseError Int Lazy.ByteString Text
  | UnknownStatusResponseError Int Lazy.ByteString
    deriving (Eq, Ord, Show)

newtype Responder a =
  Responder {
      runResponder :: HTTP.Response Lazy.ByteString -> Either BorisError a
    }

json :: Int -> (Value -> Parser a) -> Responder a
json code parser =
  Responder $ \res ->
    case (HTTP.statusCode . HTTP.responseStatus) res of
      400 ->
        (first (BorisResponseParseError 400 (HTTP.responseBody res)) $
          Decode.parse Decode.errored (HTTP.responseBody res)) >>= Left
      403 ->
        (first (BorisResponseParseError 403 (HTTP.responseBody res)) $
          Decode.parse Decode.forbidden (HTTP.responseBody res)) >>= Left
      x | x == code ->
        first (BorisResponseParseError x (HTTP.responseBody res)) $
          Decode.parse parser (HTTP.responseBody res)
      x ->
        Left $
          BorisStatusCodeError x (HTTP.responseBody res)

none :: Int -> Responder ()
none code =
  Responder $ \res ->
    case (HTTP.statusCode . HTTP.responseStatus) res of
      400 ->
        (first (BorisResponseParseError 400 (HTTP.responseBody res)) $
          Decode.parse Decode.errored (HTTP.responseBody res)) >>= Left
      403 ->
        (first (BorisResponseParseError 403 (HTTP.responseBody res)) $
          Decode.parse Decode.forbidden (HTTP.responseBody res)) >>= Left
      x | x == code ->
        pure ()
      x ->
        Left $
          BorisStatusCodeError x (HTTP.responseBody res)
