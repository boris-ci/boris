{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Resource.Dashboard (
    dashboard
  ) where


import           Airship (Resource (..), defaultResource)

import           Boris.Core.Data
import           Boris.Http.Airship
import           Boris.Http.Data
import qualified Boris.Http.Html.Template as T
import           Boris.Http.Scoreboard

import           Mismi.Amazonka (Env)

import qualified Network.HTTP.Types as HTTP

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (bimapEitherT)


dashboard :: Env -> Environment -> ConfigLocation -> Resource IO
dashboard env e c =
  defaultResource {
      allowedMethods = pure [HTTP.methodGet]

    , contentTypesProvided = return [
          (,) "text/html" $ do
             bs <- webT id . bimapEitherT renderScoreboardError id $
               fetchBrokenMasterBuilds env e c
             T.render $ T.dashboard bs
        ]
    }
