{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Http.Airship (
    htmlResponse
  , webT
  , Versioned (..)
  , PostHandler (..)
  , boom
  , notfound
  , jsonResponse
  , withVersionJson
  , withVersion'
  , setLocation
  , setLocationAbsolute
  , setResponseHeader
  , processPostMedia
  , decodeJsonBody
  , resource404
  ) where

import           Airship (ResponseBody (..), Webmachine, PostResponse (..))
import           Airship (halt, putResponseBody, request, pathInfo)
import           Airship (requestHeaders, appendRequestPath, modifyResponseHeaders)
import           Airship (entireRequestBody)
import           Airship (Resource (..), defaultResource)

import           Blaze.ByteString.Builder (toByteString)
import           Blaze.ByteString.Builder.ByteString (fromByteString)

import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Aeson (FromJSON, ToJSON, encode, object, (.=), eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Network.HTTP.Media (MediaType, mapContentMedia)
import qualified Network.HTTP.Types as HTTP

import           P

import           Prelude (Enum (..))

import           System.IO (IO, stderr)

import           Text.Blaze.Html (Html)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

import           X.Control.Monad.Trans.Either (EitherT, runEitherT)


webT :: (e -> Text) -> EitherT e IO a -> Webmachine IO a
webT render t =
  liftIO (runEitherT t) >>= \tt -> case tt of
    Left e ->
      boom . BoomApplicationInvariant . render $ e
    Right a ->
      pure a

htmlResponse :: Html -> ResponseBody
htmlResponse =
  ResponseBuilder . renderHtmlBuilder

class (Bounded a, Enum a) => Versioned a where
  versionToMedia :: a -> Text

boom :: MonadIO m => Boom -> Webmachine m a
boom b = do
  liftIO . T.hPutStrLn stderr $ renderBoom b
  putResponseBody . jsonResponse $ object [ "message" .= renderBoom b ]
  s <- pure $ case b of
    BoomNotFound _ ->
      HTTP.status404
    BoomApplicationInvariant _ ->
      HTTP.status500
  halt s

notfound :: MonadIO m => Maybe a -> Webmachine m a
notfound ma =
  case ma of
    Nothing -> do
      currentPath <- pathInfo <$> request
      boom . BoomNotFound $ T.intercalate "/" currentPath
    Just a ->
      pure a

jsonResponse :: ToJSON a => a -> ResponseBody
jsonResponse =
  ResponseBuilder . fromByteString . BSL.toStrict . encode

data Boom =
    BoomNotFound Text
  | BoomApplicationInvariant Text
    deriving (Eq, Show)

renderBoom :: Boom -> Text
renderBoom b =
  case b of
    BoomNotFound uri ->
      mconcat ["URI not found: ", uri]
    BoomApplicationInvariant msg ->
      mconcat ["Application invariant: ", msg]

withVersionJson :: Versioned v => (v -> a) -> [(MediaType, a)]
withVersionJson r =
  [("application/json", r maxBound)] <> withVersion' r

withVersion' :: Versioned v => (v -> a) -> [(MediaType, a)]
withVersion' r =
  fmap (\v -> (fromString . T.unpack . versionToMedia $ v, r v)) $ enumFromTo maxBound minBound

processPostMedia :: Monad m => [(MediaType, Webmachine m PostHandler)] -> Webmachine m (PostResponse m)
processPostMedia handlers = do
  s <- (List.lookup HTTP.hContentType . requestHeaders) <$> request
  case (s >>= mapContentMedia handlers) of
    Nothing ->
      halt HTTP.status415
    Just w ->
      w >>= \p -> pure $ case p of
        PostResponseLocationWithBody t b ->
          PostProcess $ do
            setLocation t
            putResponseBody b
        PostResponseLocation t ->
          PostProcess $
            setLocation t
        PostResponseAbsoluteLocationWithBody t b ->
          PostProcess $ do
            setLocationAbsolute t
            putResponseBody b
        PostResponseAbsoluteLocation t ->
          PostProcess $
            setLocationAbsolute t
        PostResponseSecure ->
          PostProcess $ pure ()

data PostHandler =
    PostResponseLocation [Text]
  | PostResponseLocationWithBody [Text] ResponseBody
  | PostResponseAbsoluteLocation [Text]
  | PostResponseAbsoluteLocationWithBody [Text] ResponseBody
  | PostResponseSecure

setLocation :: Monad m => [Text] -> Webmachine m ()
setLocation p =
  appendRequestPath p >>= setResponseHeader . (,) HTTP.hLocation

setLocationAbsolute :: Monad m => [Text] -> Webmachine m ()
setLocationAbsolute =
  setResponseHeader . (,) HTTP.hLocation . toByteString . HTTP.encodePathSegments

setResponseHeader :: Monad m => HTTP.Header -> Webmachine m ()
setResponseHeader s =
  modifyResponseHeaders $ \h ->
    s:(List.deleteBy (on (==) fst) s h)

decodeJsonBody :: (MonadIO m, FromJSON a) => Webmachine m a
decodeJsonBody =
  either (const $ halt HTTP.status400) return . eitherDecode
    =<< entireRequestBody
    =<< request

resource404 :: (Monad m, ToJSON a) => a -> Resource m
resource404 a = defaultResource
  { resourceExists = pure False
  , contentTypesProvided =
      pure [("application/json", pure $ jsonResponse a)]
  }
