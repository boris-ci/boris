{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Resource.Build (
    collection
  , item
  , ignore
  ) where


import           Airship (Resource (..), Webmachine, defaultResource, lookupParam, putResponseBody, appendRequestPath)

import           Boom.Airship (notfound)

import           Boris.Core.Data
import           Boris.Http.Airship
import           Boris.Http.Data
import qualified Boris.Http.Html.Template as H
import           Boris.Http.Repository
import           Boris.Http.Representation.Build
import           Boris.Http.Version
import           Boris.Store.Build (BuildData (..), BuildCancelled (..))
import qualified Boris.Store.Build as SB
import qualified Boris.Store.Index as SI
import qualified Boris.Store.Tick as ST
import           Boris.Queue (BuildQueue (..), Request (..), RequestBuild (..))
import qualified Boris.Queue as Q

import           Charlotte.Airship (PostHandler (..), withVersionJson)
import           Charlotte.Airship (processPostMedia, jsonResponse, setResponseHeader, decodeJsonBody)

import           Control.Monad.IO.Class (liftIO)

import           Data.Time (getCurrentTime, diffUTCTime)
import qualified Data.Text as T

import           Mismi (runAWS, runAWST, renderError)
import           Mismi.Amazonka (Env)

import qualified Network.HTTP.Types as HTTP

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (bimapEitherT)


collection :: Env -> Environment -> BuildQueue -> ConfigLocation -> Resource IO
collection env e q c =
  defaultResource {
      allowedMethods = pure [HTTP.methodGet, HTTP.methodPost]

    , contentTypesProvided = return . join $ [
          withVersionJson $ \v -> case v of
            V1 -> do
              b <- getBuild
              p <- getProject
              rs <- webT renderError . runAWS env $ SI.getBuildRefs e p b
              r <- fmap (GetBuilds p b) . forM rs $ \r -> do
                is <- webT renderError . runAWS env $ SI.getBuildIds e p b r
                pure $ GetBuildsDetail r is
              pure . jsonResponse $ r
        , [
            (,) "text/html" $ do
              b <- getBuild
              p <- getProject
              rs <- webT renderError . runAWS env $ SI.getBuildRefs e p b
              r <- forM rs $ \r -> do
                is <- webT renderError . runAWS env $ SI.getBuildIds e p b r
                pure $ (r, is)
              queued <- webT renderError . runAWS env $ SI.getQueued e p b
              H.render $ H.builds p b r queued
          ]
        ]

    , processPost = processPostMedia . withVersionJson $ \v -> case v of
        V1 -> do
          b <- getBuild
          p <- getProject
          repository <- webT renderConfigError (pick env c p) >>= notfound
          i <- webT id . runAWST env renderError . bimapEitherT ST.renderTickError id $ ST.next e p b
          webT id . runAWST env renderError . bimapEitherT SB.renderRegisterError id $ SB.register e p b i
          r <- getPostBuildsRef <$> decodeJsonBody
          let
            normalised = flip fmap r $ \rr -> if T.isPrefixOf "refs/" . renderRef $ rr then rr else Ref . ((<>) "refs/heads/") . renderRef $ rr
            req = RequestBuild' $ RequestBuild i p repository b normalised
          webT renderError . runAWS env $ Q.put q req
          putResponseBody . jsonResponse $ GetBuild (BuildData i p b Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
          setLocation ["builds"]
          pure $ PostResponseLocation [renderBuildId i]
    }

item :: Env -> Environment -> Resource IO
item env e =
  defaultResource {
      allowedMethods = pure [HTTP.methodGet, HTTP.methodDelete]

    , contentTypesProvided = pure . join $ [
          withVersionJson $ \v -> case v of
            V1 -> do
              i <- getBuildId
              b <- getWithHeartbeatCheck env e i
              pure . jsonResponse $ GetBuild b

        , [
            (,) "text/html" $ do
              i <- getBuildId
              b <- getWithHeartbeatCheck env e i
              H.render $ H.build b
          ]
        ]

    , deleteCompleted =
        pure False

    , deleteResource = do
        i <- getBuildId
        d <- webT id . runAWST env renderError . firstT SB.renderFetchError $
          SB.fetch e i
        webT id . bimapEitherT renderError id . runAWS env $ do
          SB.deindex e (buildDataProject d) (buildDataBuild d) i
          SB.cancel e i
    }

getWithHeartbeatCheck :: Env -> Environment -> BuildId -> Webmachine IO BuildData
getWithHeartbeatCheck env e i = do
  b <- webT id . runAWST env renderError . bimapEitherT SB.renderFetchError id $ SB.fetch e i
  case buildDataHeartbeatTime b of
    Nothing ->
      case buildDataCancelled b of
        Nothing ->
          pure b
        Just BuildNotCancelled ->
          pure b
        Just BuildCancelled ->
          pure $ b { buildDataResult = Just . fromMaybe BuildKo . buildDataResult $ b }
    Just h -> do
      now <- liftIO getCurrentTime
      if diffUTCTime now h > 120
        then do
          void . webT id . bimapEitherT renderError id . runAWS env $ SB.cancel e i
          pure $ b { buildDataResult = Just . fromMaybe BuildKo . buildDataResult $ b }
        else
          pure b

ignore :: Env -> Environment -> Resource IO
ignore env e =
  defaultResource {
      allowedMethods = pure [HTTP.methodPut]

    , contentTypesAccepted = return . withVersionJson $ \v -> case v of
        V1 -> do
          b <- getBuild
          p <- getProject
          PutBuildIgnore i <- decodeJsonBody
          webT renderError . runAWS env $ SI.setBuildDisabled e p b i

    }

getBuild :: Webmachine IO Build
getBuild =
  Build <$> lookupParam "build-name"

getProject :: Webmachine IO Project
getProject =
  Project <$> lookupParam "project-name"

getBuildId :: Webmachine IO BuildId
getBuildId =
  BuildId <$> lookupParam "build-id"

setLocation :: [Text] -> Webmachine IO ()
setLocation p =
  appendRequestPath p >>= setResponseHeader . (,) HTTP.hLocation
