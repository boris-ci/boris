{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Resource.Project (
    collection
  , item
  ) where


import           Airship (Resource (..), Webmachine, defaultResource, lookupParam)

import           Boris.Core.Data
import           Boris.Http.Airship
import           Boris.Http.Data
import           Boris.Http.Repository (list, renderConfigError)
import           Boris.Http.Representation.Project
import           Boris.Http.Version
import qualified Boris.Store.Index as SI

import           Charlotte.Airship (withVersionJson)
import           Charlotte.Airship (jsonResponse)

import           Mismi (runAWS, renderError)
import           Mismi.Amazonka (Env)

import qualified Network.HTTP.Types as HTTP

import           P

import           System.IO (IO)

collection :: Env -> ConfigLocation -> Resource IO
collection env c =
  defaultResource {
      allowedMethods = pure [HTTP.methodGet]

    , contentTypesProvided = return . withVersionJson $ \v -> case v of
        V1 -> do
          ps <- webT renderConfigError $
            list env c

          return . jsonResponse $ GetProjects ps
    }

item :: Env -> Environment -> Resource IO
item env e =
  defaultResource {
      allowedMethods = pure [HTTP.methodGet]

    , contentTypesProvided = pure . withVersionJson $ \v -> case v of
        V1 -> do
          p <- getProject
          bs <- webT renderError . runAWS env $ SI.getProjects e p
          pure . jsonResponse $ GetProject p bs
    }

getProject :: Webmachine IO Project
getProject =
  Project <$> lookupParam "project-name"
