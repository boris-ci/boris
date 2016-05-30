{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Boris.Http.Resource.Static (
    css
  ) where


import           Airship (Resource (..), ResponseBody (..), defaultResource)

import           Blaze.ByteString.Builder.ByteString (fromByteString)

import           Data.FileEmbed (embedFile)

import qualified Network.HTTP.Types as HTTP

import           P

import           System.IO (IO)


css :: Resource IO
css =
  defaultResource {
      allowedMethods = pure [
           HTTP.methodGet
         ]

    , contentTypesProvided = pure [
          (,) "text/css" $ do
             return . ResponseBuilder . fromByteString $ $(embedFile "css/boris.css")
        ]
    }
