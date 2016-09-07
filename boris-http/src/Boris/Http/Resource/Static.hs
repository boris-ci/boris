{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Boris.Http.Resource.Static (
    staticMiddleware
  ) where


import           Network.Wai.Application.Static (embeddedSettings, staticApp)

import           Data.FileEmbed (embedDir)

import           Network.Wai (Application, Middleware, Request (..))

import           P


staticMiddleware :: Middleware
staticMiddleware app req resp =
  case isPrefixOf ["assets"] (pathInfo req) of
    False ->
      app req resp
    True ->
      assetsApp req { pathInfo = drop 1 $ pathInfo req } resp

assetsApp :: Application
assetsApp =
  staticApp $ embeddedSettings $(embedDir "assets")
