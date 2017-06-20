{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Resource.Scoreboard (
    scoreboard
  ) where


import           Airship (Resource (..), Webmachine, defaultResource)

import           Boris.Core.Data
import           Boris.Http.Airship
import           Boris.Http.Representation.Scoreboard
import           Boris.Http.Scoreboard
import qualified Boris.Store.Results as SR

import           Charlotte.Airship (jsonResponse)

import           Mismi.Amazonka (Env)

import qualified Network.HTTP.Types as HTTP

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (bimapEitherT)


scoreboard :: Env -> Environment -> Resource IO
scoreboard env e =
  defaultResource {
      allowedMethods = pure [HTTP.methodGet]

    , contentTypesProvided = return [
          ("application/json", jsonResponse . GetScoreboard <$> scoreboardWeb env e)
        , ("application/vnd.ambiata.boris.v1+json", jsonResponse . GetScoreboard <$> scoreboardWeb env e)
        , ("text/html", htmlResponse . scoreboardHtml <$> scoreboardWeb env e)
        ]

    }

scoreboardWeb :: Env -> Environment -> Webmachine IO [SR.Result]
scoreboardWeb env e =
  webT id . bimapEitherT renderScoreboardError id $ fetchLatestMasterBuilds env e
