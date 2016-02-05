{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Resource.Build (
    collection
  , item
  ) where


import           Airship (Resource (..), Webmachine, defaultResource, lookupParam, halt, putResponseBody, appendRequestPath)

import           Boom.Data (Boom (..))
import           Boom.Airship (boom, notfound)

import           Boris.Core.Data
import           Boris.Http.Data
import           Boris.Http.Repository
import           Boris.Http.Version
import           Boris.Store.Build (BuildData (..))
import qualified Boris.Store.Build as SB
import qualified Boris.Store.Tick as ST
import           Boris.Queue (BuildQueue (..), Request (..))
import qualified Boris.Queue as Q

import           Charlotte.Airship (PostHandler (..), withVersionJson)
import           Charlotte.Airship (processPostMedia, jsonResponse, setResponseHeader)

import           Control.Monad.IO.Class (liftIO)

import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Text (Text)

import           Jebediah.Data (GroupName (..), StreamName (..))

import           Mismi (runAWS, runAWST, renderError)
import           Mismi.Amazonka (Env)

import qualified Network.HTTP.Types as HTTP

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, runEitherT, bimapEitherT)

collection :: Env -> Environment -> BuildQueue -> ConfigLocation -> Resource IO
collection env e q c =
  defaultResource {
      allowedMethods = pure [HTTP.methodPost]

    , contentTypesProvided = return . withVersionJson $ \v -> case v of
        V1 ->
          halt HTTP.status500

    , processPost = processPostMedia . withVersionJson $ \v -> case v of
        V1 -> do
          b <- getBuild
          p <- getProject
          repository <- webT renderConfigError (pick env c p) >>= notfound
          i <- webT id . runAWST env renderError . bimapEitherT ST.renderTickError id $ ST.next e p b
          webT id . runAWST env renderError . bimapEitherT SB.renderRegisterError id $ SB.register e p b i
          let req = Request i p repository b Nothing -- FIX COMPLETE ref needs to be parsed from body
          webT renderError . runAWS env $ Q.put q req
          putResponseBody . jsonResponse $ GetBuild (BuildData i p b Nothing Nothing Nothing Nothing Nothing Nothing)
          setLocation ["builds"]
          pure $ PostResponseLocation [renderBuildId i]
    }

item :: Env -> Environment -> Resource IO
item env e =
  defaultResource {
      allowedMethods = pure [HTTP.methodGet]

    , contentTypesProvided = pure . withVersionJson $ \v -> case v of
        V1 -> do
          i <- getBuildId
          b <- webT id . runAWST env renderError . bimapEitherT SB.renderFetchError id $ SB.fetch e i
          return . jsonResponse $ GetBuild b
    }

getBuild :: Webmachine IO Build
getBuild = do
  Build <$> lookupParam "build-name"

getProject :: Webmachine IO Project
getProject = do
  Project <$> lookupParam "project-name"

getBuildId :: Webmachine IO BuildId
getBuildId = do
  BuildId <$> lookupParam "build-id"

webT :: (e -> Text) -> EitherT e IO a -> Webmachine IO a
webT render t =
  liftIO (runEitherT t) >>= \tt -> case tt of
    Left e ->
      boom . BoomApplicationInvariant . render $ e
    Right a ->
      pure a

newtype GetBuild =
  GetBuild BuildData

instance ToJSON GetBuild where
  toJSON (GetBuild b) =
    object [
        "build_id" .= (renderBuildId . buildDataId) b
      , "project" .= (renderProject . buildDataProject) b
      , "build" .= (renderBuild . buildDataBuild) b
      , "ref" .= (fmap renderRef . buildDataRef) b
      , "queued" .= buildDataQueueTime b
      , "started" .= buildDataStartTime b
      , "completed" .= buildDataEndTime b
      , "result" .= (flip fmap (buildDataResult b) $ \bb -> case bb of BuildOk -> True; BuildKo -> False)
      , "log" .= (flip fmap (buildDataLog b) $ \l -> object ["group" .= (unGroupName . SB.logGroup) l, "stream" .= (unStreamName . SB.logStream) l])
      ]

setLocation :: [Text] -> Webmachine IO ()
setLocation p =
  appendRequestPath p >>= setResponseHeader . (,) HTTP.hLocation
