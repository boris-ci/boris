{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Request (
    Request (..)
  , Requester (..)

  , none
  , json
  ) where

import           Boris.Prelude
import           Boris.Client.Response (Responder (..))

import qualified Data.Aeson as Aeson
import           Data.Aeson (Value)
import           Data.Text (Text)

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP


data Request a =
  Request {
      requestMethod :: HTTP.StdMethod
    , requestPath :: Text
    , requestResponder :: Responder a
    , requestRequester :: Requester
    }


newtype Requester =
  Requester {
      runRequester :: HTTP.Request -> HTTP.Request
    }


none :: Requester
none =
  Requester $ \request ->
    request {
        HTTP.requestHeaders = [
            ("Accept", "application/json")
          ] ++ HTTP.requestHeaders request
      }


json :: Value -> Requester
json value =
  Requester $ \request ->
    request {
        HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode value)
      , HTTP.requestHeaders = [
            ("Content-Type", "application/json")
          , ("Accept", "application/json")
          ] ++ HTTP.requestHeaders request
      }
