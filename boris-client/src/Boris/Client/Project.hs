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


list :: Request [Project]
list =
  Request HTTP.GET "project"
    (Response.json 200 $ Decode.wrapper getProjects)
    Request.none

fetch :: Project -> Request [Build]
fetch p =
  Request HTTP.GET (mconcat ["project/", renderProject p])
    (Response.json 200 $ Decode.wrapper getProjectBuilds)
    Request.none

discover :: Project -> Request ()
discover p =
  Request HTTP.POST (mconcat ["project", renderProject p])
    (Response.none 202)
    Request.none
