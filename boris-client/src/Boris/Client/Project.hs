{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Project (
    list
  , fetch
  , discover
  ) where

import qualified Boris.Client.Response as Response
import           Boris.Client.Request (Request (..))
import qualified Boris.Client.Request as Request
import qualified Boris.Client.Serial.Decode as Decode
import           Boris.Core.Data.Build
import           Boris.Core.Data.Project
import           Boris.Prelude
import           Boris.Representation.ApiV1

import qualified Network.HTTP.Types as HTTP


list :: Request [ProjectName]
list =
  Request HTTP.GET "project"
    (Response.json 200 $ Decode.wrapper getProjects)
    Request.none

fetch :: ProjectName -> Request [BuildName]
fetch p =
  Request HTTP.GET (mconcat ["project/", renderProjectName p])
    (Response.json 200 $ Decode.wrapper getProjectBuilds)
    Request.none

discover :: ProjectName -> Request ()
discover p =
  Request HTTP.POST (mconcat ["project", renderProjectName p])
    (Response.none 202)
    Request.none
