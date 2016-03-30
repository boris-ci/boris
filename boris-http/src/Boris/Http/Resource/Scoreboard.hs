{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Resource.Scoreboard (
    scoreboard
  ) where


import           Airship (Resource (..), Webmachine, defaultResource)

import           Boris.Core.Data
import           Boris.Http.Airship
import           Boris.Http.Data
import           Boris.Http.Representation.Scoreboard
import           Boris.Http.Scoreboard
import qualified Boris.Store.Build as SB

import           Charlotte.Airship (jsonResponse)

import           Mismi.Amazonka (Env)

import qualified Network.HTTP.Types as HTTP

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (bimapEitherT)


scoreboard :: Env -> Environment -> ConfigLocation -> Resource IO
scoreboard env e c =
  defaultResource {
      allowedMethods = pure [HTTP.methodGet]

    , contentTypesProvided = return [
          ("application/json", jsonResponse . GetScoreboard <$> scoreboardWeb env e c)
        , ("application/vnd.ambiata.boris.v1+json", jsonResponse . GetScoreboard <$> scoreboardWeb env e c)
        , ("text/html", htmlResponse . scoreboardHtml <$> scoreboardWeb env e c)
        ]
    }

scoreboardWeb :: Env -> Environment -> ConfigLocation -> Webmachine IO [SB.BuildData]
scoreboardWeb env e c =
  webT id . bimapEitherT renderScoreboardError id $ fetchLatestMasterBuilds env e c
