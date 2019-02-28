{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Client.Response (
    ResponseError (..)

  , Responder (..)

  , json
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
      runResponder :: HTTP.Response Lazy.ByteString -> Either SmithError a
    }

json :: Int -> (Value -> Parser a) -> Responder a
json code parser =
  Responder $ \res ->
    case (HTTP.statusCode . HTTP.responseStatus) res of
      400 ->
        (first (SmithResponseParseError 400 (HTTP.responseBody res)) $
          Decode.parse Decode.errored (HTTP.responseBody res)) >>= Left
      403 ->
        (first (SmithResponseParseError 403 (HTTP.responseBody res)) $
          Decode.parse Decode.forbidden (HTTP.responseBody res)) >>= Left
      x | x == code ->
        first (SmithResponseParseError x (HTTP.responseBody res)) $
          Decode.parse parser (HTTP.responseBody res)
      x ->
        Left $
          SmithStatusCodeError x (HTTP.responseBody res)
