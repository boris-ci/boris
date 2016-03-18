{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Representation.Build (
    GetBuild (..)
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
