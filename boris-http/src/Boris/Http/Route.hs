{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Route (
    boris
  , borisReadonly
  ) where

import           Airship (RoutingSpec, var, (#>), (</>))

import           Boris.Core.Data
import           Boris.Http.Data
import qualified Boris.Http.Resource.Build as Build
import qualified Boris.Http.Resource.Project as Project
import qualified Boris.Http.Resource.Scoreboard as Scoreboard
import           Boris.Queue (BuildQueue (..))

import           Mismi.Amazonka (Env)

import           System.IO (IO)

boris :: Env -> Environment -> BuildQueue -> ConfigLocation -> RoutingSpec IO ()
boris env e q c = do
--  root #> TODO
  "project" #> Project.collection env c
  "project" </> var "project-name" #> Project.item env e q c
  "project" </> var "project-name" </> "build" </> var "build-name" #> Build.collection env e q c
--  "project" </> var "project-name" </> "commit" </> var "commit-hash" #> TODO
  "build" </> var "build-id" #> Build.item env e
  "scoreboard" #> Scoreboard.scoreboard env e c

borisReadonly :: Env -> Environment -> ConfigLocation -> RoutingSpec IO ()
borisReadonly env e c = do
  "scoreboard" #> Scoreboard.scoreboard env e c
