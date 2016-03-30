{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Http.Airship (
    htmlResponse
  , webT
  ) where

import           Airship (ResponseBody (..), Webmachine)

import           Boom.Data (Boom (..))
import           Boom.Airship (boom)

import           Control.Monad.IO.Class (liftIO)

import           Data.Text (Text)

import           P

import           System.IO (IO)

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
