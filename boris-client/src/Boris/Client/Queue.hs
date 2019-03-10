{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Queue (
    next
  ) where


import           Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Boris.Client.Response as Response
import           Boris.Client.Request (Request (..))
import qualified Boris.Client.Request as Request
import qualified Boris.Client.Serial.Encode as Encode
import qualified Boris.Client.Serial.Decode as Decode
import           Boris.Core.Data.Build
import           Boris.Core.Data.Discover
import           Boris.Core.Data.Keyed
import           Boris.Core.Data.Project
import           Boris.Prelude
import           Boris.Representation.ApiV1

import qualified Data.Text as Text

import qualified Network.HTTP.Types as HTTP

next :: Request (Maybe (Either (Keyed DiscoverId Discover) (Keyed BuildId Build)))
next =
  Request HTTP.POST "queue"
    (Response.option . Response.json 200 $ Decode.wrapper getNext)
    (Request.json . Encode.auto $ ())
