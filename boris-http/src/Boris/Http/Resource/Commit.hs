{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Resource.Commit (
    item
  ) where


import           Airship (Resource (..), Webmachine, defaultResource, lookupParam)

import           Boris.Core.Data
import           Boris.Http.Airship
import qualified Boris.Http.Html.Template as H
import           Boris.Http.Representation.Commit
import           Boris.Http.Version
import qualified Boris.Store.Build as SB
import qualified Boris.Store.Index as SI

import           Mismi (runAWS, runAWST, renderError)
import           Mismi.Amazonka (Env)

import qualified Network.HTTP.Types as HTTP

import           P

import           System.IO (IO)


item :: Env -> Environment -> Resource IO
item env e =
  defaultResource {
      allowedMethods = pure [HTTP.methodGet]

      , contentTypesProvided = return . join $ [
          withVersionJson $ \v -> case v of
            V1 -> do
              p <- getProject
              c <- getCommit
              bs <- webT renderError . runAWS env $ SI.getProjectCommitBuildIds e p c
              pure . jsonResponse $ GetCommit p bs
        , [
            (,) "text/html" $ do
              p <- getProject
              c <- getCommit
              bs <- webT renderError . runAWS env $ SI.getProjectCommitBuildIds e p c
              bds <- webT id . runAWST env renderError . firstT SB.renderFetchError . for bs $ SB.fetch e
              H.render $ H.commit p c bds
          ]
        ]
    }

getProject :: Webmachine IO Project
getProject =
  Project <$> lookupParam "project-name"

getCommit :: Webmachine IO Commit
getCommit =
  Commit <$> lookupParam "commit-hash"
