{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Resource.Log (
    item
  ) where


import           Airship (Resource (..), Webmachine, ResponseBody (..), defaultResource, lookupParam, halt)

import           Boris.Core.Data
import           Boris.Http.Airship
import           Boris.Http.Version
import           Boris.Store.Build
import qualified Boris.Store.Build as SB

import           Blaze.ByteString.Builder (fromByteString)

import           Charlotte.Airship (withVersionJson)

import           Control.Lens (view)

import           Data.Conduit (Source, (=$=), transPipe, runConduit)
import qualified Data.Conduit.List as CL

import qualified Data.Text.Encoding as T

import           Jebediah.Data (Following (..))
import           Jebediah.Control (retrieveLogStream')

import           Mismi (AWS, rawRunAWS, runAWST, renderError)
import           Mismi.Amazonka (Env)
import qualified Mismi.CloudwatchLogs.Amazonka as CW


import qualified Network.HTTP.Types as HTTP

import           P

import           System.IO (IO)

item :: Env -> Environment -> Resource IO
item env e =
  defaultResource {
      allowedMethods = pure [
           HTTP.methodGet
         ]

    , contentTypesProvided = pure . withVersionJson $ \v -> case v of
        V1 -> do
          i <- getBuildId

          l <- webT id . runAWST env renderError . firstT renderFetchError $
            SB.fetch e i

          case buildDataLog l of
            Nothing ->
              halt HTTP.status404

            Just ld -> do
              pure . ResponseStream $ \send flush ->
                runConduit $
                  transPipe (rawRunAWS env) (source ld) =$= CL.mapM_ (\t -> do
                    send (fromByteString . T.encodeUtf8 $ t)
                    send (fromByteString "\n")
                    flush)
    }

source :: LogData -> Source AWS Text
source (LogData gname sname) =
  retrieveLogStream' gname sname Nothing Nothing Nothing NoFollow
    =$= CL.mapFoldable (view CW.oleMessage)

getBuildId :: Webmachine IO BuildId
getBuildId =
  BuildId <$> lookupParam "build-id"
