{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Resource.Queue (
    collection
  ) where


import           Airship (Resource (..), defaultResource)

import           Boris.Http.Airship
import           Boris.Http.Representation.Queue
import           Boris.Http.Version
import           Boris.Queue (BuildQueue (..))
import qualified Boris.Queue as Q

import           Charlotte.Airship (withVersionJson, jsonResponse)

import           Mismi (runAWS, renderError)
import           Mismi.Amazonka (Env)

import qualified Network.HTTP.Types as HTTP

import           P

import           System.IO (IO)


collection :: Env -> BuildQueue -> Resource IO
collection env q =
  defaultResource {
      allowedMethods = pure [HTTP.methodGet]

    , contentTypesProvided = return . join $ [
          withVersionJson $ \v -> case v of
            V1 -> do
              s <- webT renderError . runAWS env $ Q.size q
              pure . jsonResponse $ GetQueue s
        ]
    }
