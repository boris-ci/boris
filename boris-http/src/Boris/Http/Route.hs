{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Route (
    boris
  , borisReadonly
  ) where

import           Airship (RoutingSpec, root, var, (#>), (</>))

import           Boris.Core.Data
import           Boris.Http.Data
import qualified Boris.Http.Resource.Build as Build
import qualified Boris.Http.Resource.Commit as Commit
import qualified Boris.Http.Resource.Dashboard as Dashboard
import qualified Boris.Http.Resource.Log as Log
import qualified Boris.Http.Resource.Project as Project
import qualified Boris.Http.Resource.Scoreboard as Scoreboard
import qualified Boris.Http.Resource.Status as Status
import qualified Boris.Http.Resource.Queue as Queue
import           Boris.Queue (BuildQueue (..))

import           Mismi.Amazonka (Env)

import           System.IO (IO)

boris :: ClientLocale -> Env -> Environment -> BuildQueue -> ConfigLocation -> RoutingSpec IO ()
boris l env e q c = do
  root #> Dashboard.dashboard env q
  "status" #> Status.status env e
  "queue" #> Queue.collection env q
  "project" #> Project.collection env c
  "project" </> var "project-name" #> Project.item env e q c
  "project" </> var "project-name" </> "build" </> var "build-name" #> Build.collection env e q c
  "project" </> var "project-name" </> "build" </> var "build-name" </> "ignore" #> Build.ignore env e
  "project" </> var "project-name" </> "commit" </> var "commit-hash" #> Commit.item env e
  "build" </> var "build-id" #> Build.item l env e
  "build" </> var "build-id" </> "log" #> Log.item env e
  "build" </> var "build-id" </> "cancel" #> Build.cancel env e
  "scoreboard" #> Scoreboard.scoreboard env e

borisReadonly :: Env -> Environment -> RoutingSpec IO ()
borisReadonly env e =
  "scoreboard" #> Scoreboard.scoreboard env e
