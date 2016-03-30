{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Http.Airship (
    htmlResponse
  ) where

import           Airship (ResponseBody (..))

import           P

import           Text.Blaze.Html (Html)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)


htmlResponse :: Html -> ResponseBody
htmlResponse =
  ResponseBuilder . renderHtmlBuilder
