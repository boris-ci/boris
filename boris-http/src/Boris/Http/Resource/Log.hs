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

import           Charlotte.Airship (Versioned, withVersion')

import           Data.Conduit (Source, (=$=), runConduit)
import qualified Data.Conduit.List as CL

import qualified Data.Text.Encoding as T

import           Jebediah.Data (Query (..), Following (..), Log (..))
import qualified Jebediah.Conduit as J

import           Mismi (runAWST, renderError)
import           Mismi.Amazonka (Env)

import           Network.HTTP.Media.MediaType (MediaType)
import qualified Network.HTTP.Types as HTTP

import           P

import           System.IO (IO)

item :: Env -> Environment -> Resource IO
item env e =
  defaultResource {
      allowedMethods = pure [
           HTTP.methodGet
         ]

    , contentTypesProvided = pure . withVersionText $ \v -> case v of
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
                  source env ld =$= CL.mapM_ (\t -> do
                    send (fromByteString . T.encodeUtf8 $ t)
                    send (fromByteString "\n")
                    flush)
    }

withVersionText :: Versioned v => (v -> a) -> [(MediaType, a)]
withVersionText r =
  [("text/plain; charset=utf-8", r maxBound)] <> withVersion' r

source :: Env -> LogData -> Source IO Text
source env (LogData gname sname) =
  J.source env gname sname Everything NoFollow
    =$= J.unclean
    =$= CL.map logChunk

getBuildId :: Webmachine IO BuildId
getBuildId =
  BuildId <$> lookupParam "build-id"
