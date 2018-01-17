{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Resource.Authentication  (
    login
  , callback
  ) where


import           Airship (Resource (..), Webmachine, defaultResource, lookupParam, lookupParam', putResponseBody, halt)

import           Boris.Core.Data
import           Boris.Http.Airship
import           Boris.Http.Data
import qualified Boris.Http.Html.Template as H
import           Boris.Http.Form
import           Boris.Http.Repository
import           Boris.Http.Representation.Build
import           Boris.Http.Version
import           Boris.Store.Build (BuildData (..), BuildCancelled (..))
import qualified Boris.Store.Build as SB
import qualified Boris.Store.Index as SI
import qualified Boris.Store.Tick as ST
import           Boris.Queue (BuildQueue (..), Request (..), RequestBuild (..))
import qualified Boris.Queue as Q

import           Control.Monad.IO.Class (liftIO)

import           Data.List (lookup)
import           Data.Time (getCurrentTime, diffUTCTime)
import qualified Data.Text as T

import           Mismi (runAWS, runAWST, renderError)
import           Mismi.Amazonka (Env)

import qualified Network.HTTP.Types as HTTP

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (bimapEitherT)


login :: GithubClient -> Resource IO
login client =
  defaultResource {
      allowedMethods = pure [HTTP.methodGet]

    , contentTypesProvided = return . join $ [
          [
            (,) "text/html" $ do
              H.render $ H.login client
          ]
        ]
    }

callback :: GithubClient -> Resource IO
callback client =
  defaultResource {
      allowedMethods = pure [HTTP.methodGet]

    , contentTypesProvided = return . join $ [
          [
            (,) "text/html" $ do
              H.render $ H.login client
          ]
        ]
    }





getRedirect :: Webmachine IO (Maybe Text)
getRedirect =
  lookupParam' "redirect"
