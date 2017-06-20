{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Resource.Status (
    status
  ) where


import           Airship (Resource (..), defaultResource)

import           Boris.Core.Data
import           Boris.Http.Airship
import qualified Boris.Http.Html.Template as T
import           Boris.Http.Scoreboard

import           Mismi.Amazonka (Env)

import qualified Network.HTTP.Types as HTTP

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (bimapEitherT)


status :: Env -> Environment -> Resource IO
status env e =
  defaultResource {
      allowedMethods = pure [HTTP.methodGet]

    , contentTypesProvided = return [
          (,) "text/html" $ do
             bs <- webT id . bimapEitherT renderScoreboardError id $
               fetchBrokenMasterBuilds env e
             T.render $ T.status bs
        ]
    }
