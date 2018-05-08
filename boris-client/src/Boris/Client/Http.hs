{-# LANGUAGE GADTs #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Http (
    BorisHttpClientError (..)
  , renderBorisHttpClientError
  , get
  , post
  , post_
  , put
  , delete
  ) where


import           Data.Aeson (FromJSON (..), ToJSON (..), encode)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import           Network.Api.Support (RequestTransformer, addHeader, runRequestWith, basicResponder, parseBodyWith)
import qualified Network.HTTP.Client as H
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types (Status (..))
import qualified Network.HTTP.Types as HTTP

import           P

import           Snooze.Balance.Control (BalanceConfig, BalanceError (..), runBalanceT)
import           Snooze.Balance.Http (httpBalanced)
import           Snooze.Json (decodeResponse)
import           Snooze.Url (encodePathSegmentsBS)

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, bimapEitherT, hoistEither, left, newEitherT)

data BorisHttpClientError =
    BorisHttpClientBalanceError BalanceError
  | BorisHttpClientDecodeError Text
  | BorisHttpClientUnhandledResponseError (H.Response BL.ByteString)
    deriving (Show)

borisVersion :: ByteString
borisVersion =
  "application/vnd.boris.v1+json"

delete :: BalanceConfig -> [Text] -> EitherT BorisHttpClientError IO ()
delete b url = do
  res <- bimapEitherT BorisHttpClientBalanceError id . newEitherT . runBalanceT b . httpBalanced $ \r -> r {
      H.path = encodePathSegmentsBS url
    , H.requestHeaders = [(HTTP.hContentType, borisVersion)]
    , H.method = HTTP.methodDelete
    }
  case H.responseStatus res of
    Status 202 _ ->
      pure ()
    _ ->
      left $ BorisHttpClientUnhandledResponseError res

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
    Status 200 _ ->
      hoistEither . first BorisHttpClientDecodeError $ decodeResponse res
    _ ->
      left $ BorisHttpClientUnhandledResponseError res

post_ :: BalanceConfig -> [Text] -> EitherT BorisHttpClientError IO ()
post_ b url = do
  res <- bimapEitherT BorisHttpClientBalanceError id . newEitherT . runBalanceT b . httpBalanced $ \r -> r {
      H.path = encodePathSegmentsBS url
    , H.requestHeaders = [(HTTP.hContentType, borisVersion)]
    , H.method = HTTP.methodPost
    }
  case H.responseStatus res of
    Status 202 _ ->
      pure ()
    _ ->
      left $ BorisHttpClientUnhandledResponseError res

put :: ToJSON a => BalanceConfig -> [Text] -> a -> EitherT BorisHttpClientError IO ()
put b url a = do
  res <- bimapEitherT BorisHttpClientBalanceError id . newEitherT . runBalanceT b . httpBalanced $ \r -> r {
      H.path = encodePathSegmentsBS url
    , H.requestHeaders = [(HTTP.hContentType, borisVersion)]
    , H.method = HTTP.methodPut
    , H.requestBody = H.RequestBodyLBS . encode $ a
    }
  case H.responseStatus res of
    Status 204 _ ->
      pure ()
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

data BorisRequest a where
  BorisRequestAccept :: (FromJSON a) => HTTP.StdMethod -> Text -> RequestTransformer -> BorisRequest a

data BorisResponse a =
    BorisResponseError
  | BorisResponseSuccess a
  deriving (Eq, Show)

data BorisClientSettings =
  BorisClientSettings H.Manager Text

newBorisClientSettings :: IO BorisClientSettings
newBorisClientSettings =
  BorisClientSettings <$> H.newManager tlsManagerSettings <*> pure "http://localhost:10080"

request :: BorisClientSettings -> BorisRequest a -> IO (BorisResponse a)
request (BorisClientSettings manager url) req =
  case req of
    BorisRequestAccept method path transform ->
      runRequestWith manager method (url <> path) (mconcat [
          addHeader ("Accept", "application.json")
        , transform
        ]) (basicResponder acceptResponder)

acceptResponder :: FromJSON a => Int -> BL.ByteString -> BorisResponse a
acceptResponder n body =
  case n of
    202 ->
      parseBodyWith body (\_t -> BorisResponseError) (\_t -> BorisResponseError) BorisResponseSuccess
    200 ->
      parseBodyWith body (\_t -> BorisResponseError) (\_t -> BorisResponseError) BorisResponseSuccess
    _ ->
      BorisResponseError

renderBorisHttpClientError :: BorisHttpClientError -> Text
renderBorisHttpClientError err =
  case err of
    BorisHttpClientBalanceError (BalanceTimeout es) ->
      mconcat ["A timeout occured attempting to make a request: ", T.intercalate ", " (fmap (T.pack . show) es)]
    BorisHttpClientDecodeError t ->
      mconcat ["Could not decode response from server: ", t]
    BorisHttpClientUnhandledResponseError res ->
      mconcat ["Unhandled response from server: ", T.pack . show $ H.responseStatus res]
