{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Resource.Project (
    collection
  , item
  ) where


import           Airship (Resource (..), Webmachine, defaultResource, lookupParam, halt)

import           Boom.Airship (notfound)

import           Boris.Core.Data
import           Boris.Http.Airship
import           Boris.Http.Data
import qualified Boris.Http.Html.Template as H
import           Boris.Http.Repository (pick, list, renderConfigError)
import           Boris.Http.Representation.Project
import           Boris.Http.Version
import qualified Boris.Store.Index as SI
import qualified Boris.Store.Tick as ST
import           Boris.Queue (BuildQueue (..), Request (..), RequestDiscover (..))
import qualified Boris.Queue as Q

import           Charlotte.Airship (withVersionJson)
import           Charlotte.Airship (jsonResponse)
import           Charlotte.Airship (processPostMedia)

import           Mismi (runAWS, runAWST, renderError)
import           Mismi.Amazonka (Env)

import qualified Network.HTTP.Types as HTTP

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (bimapEitherT)


collection :: Env -> ConfigLocation -> Resource IO
collection env c =
  defaultResource {
      allowedMethods = pure [HTTP.methodGet]

    , contentTypesProvided = return . join $ [
          withVersionJson $ \v -> case v of
            V1 -> do
              ps <- webT renderConfigError $
                list env c
              return . jsonResponse $ GetProjects ps
        , [
            (,) "text/html" $ do
              ps <- webT renderConfigError $
                list env c
              H.render $ H.projects ps
          ]
        ]
    }

item :: Env -> Environment -> BuildQueue -> ConfigLocation -> Resource IO
item env e q c =
  defaultResource {
      allowedMethods = pure [HTTP.methodGet, HTTP.methodPost]

      , contentTypesProvided = return . join $ [
          withVersionJson $ \v -> case v of
            V1 -> do
              p <- getProject
              bs <- webT renderError . runAWS env $ SI.getProjects e p
              pure . jsonResponse $ GetProject p bs
        , [
            (,) "text/html" $ do
              p <- getProject
              bs <- webT renderError . runAWS env $ SI.getProjects e p
              H.render $ H.project p bs
          ]
        ]

    , processPost = processPostMedia . withVersionJson $ \v -> case v of
        V1 -> do
          p <- getProject
          repository <- webT renderConfigError (pick env c p) >>= notfound
          i <- webT id . runAWST env renderError . bimapEitherT ST.renderTickError id $ ST.next e p (Build ":discover:")
          let req = RequestDiscover' $ RequestDiscover i p repository
          webT renderError . runAWS env $ Q.put q req
          halt HTTP.status202
    }

getProject :: Webmachine IO Project
getProject =
  Project <$> lookupParam "project-name"
