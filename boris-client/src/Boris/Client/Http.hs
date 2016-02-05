{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Http (
    BorisHttpClientError (..)
  , renderBorisHttpClientError
  , get
  , post
  ) where


import           Data.Aeson (FromJSON (..), ToJSON (..), encode)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Text (Text)
import qualified Data.Text as T

import qualified Network.HTTP.Client as H
import           Network.HTTP.Types (Status (..))
import qualified Network.HTTP.Types as HTTP

import           P

import           Snooze.Balance.Control (BalanceConfig, BalanceError (..), runBalanceT)
import           Snooze.Balance.Http (httpBalanced)
import           Snooze.Url (encodePathSegmentsBS)
import           Snooze.Json (decodeResponse)

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, newEitherT, bimapEitherT, hoistEither, left)

data BorisHttpClientError =
    BorisHttpClientBalanceError BalanceError
  | BorisHttpClientDecodeError Text
  | BorisHttpClientUnhandledResponseError (H.Response BL.ByteString)

borisVersion :: ByteString
borisVersion =
  "application/vnd.ambiata.boris.v1+json"

post :: (ToJSON a, FromJSON b) => BalanceConfig -> [Text] -> a -> EitherT BorisHttpClientError IO b
post b url a = do
  res <- bimapEitherT BorisHttpClientBalanceError id . newEitherT . runBalanceT b . httpBalanced $ \r -> r {
      H.path = encodePathSegmentsBS url
    , H.requestHeaders = [(HTTP.hContentType, borisVersion)]
    , H.method = HTTP.methodPost
    , H.requestBody = H.RequestBodyLBS . encode $ a
    }
  case H.responseStatus res of
    Status 201 _ ->
      hoistEither . first BorisHttpClientDecodeError $ decodeResponse res
    _ ->
      left $ BorisHttpClientUnhandledResponseError res

get :: FromJSON a => BalanceConfig -> [Text] -> EitherT BorisHttpClientError IO (Maybe a)
get b url = do
  res <- bimapEitherT BorisHttpClientBalanceError id . newEitherT . runBalanceT b . httpBalanced $ \r -> r {
      H.path = encodePathSegmentsBS url
    , H.requestHeaders = [(HTTP.hAccept, borisVersion)]
    , H.method = HTTP.methodGet
    }
  case H.responseStatus res of
    Status 404 _ ->
      pure Nothing
    Status 200 _ ->
      hoistEither . first BorisHttpClientDecodeError $ decodeResponse res
    _ ->
     left $ BorisHttpClientUnhandledResponseError res

renderBorisHttpClientError :: BorisHttpClientError -> Text
renderBorisHttpClientError err =
  case err of
    BorisHttpClientBalanceError (BalanceTimeout es) ->
      mconcat ["A timeout occured attempting to make a request: ", T.intercalate ", " (fmap (T.pack . show) es)]
    BorisHttpClientDecodeError t ->
      mconcat ["Could not decode response from server: ", t]
    BorisHttpClientUnhandledResponseError res ->
      mconcat ["Unhandled response from server: ", T.pack . show $ H.responseStatus res]
