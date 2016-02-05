{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http (
    boris
  ) where

import           Airship (RoutingSpec, var, (#>), (</>))

import           Boris.Core.Data
import           Boris.Http.Data
import qualified Boris.Http.Resource.Build as Build
import           Boris.Queue (BuildQueue (..))

import           Mismi.Amazonka (Env)

import           System.IO (IO)

boris :: Env -> Environment -> BuildQueue -> ConfigLocation -> RoutingSpec IO ()
boris env e q c = do
  "project" </> var "project-name" </> "build" </> var "build-name" #> Build.collection env e q c
  "build" </> var "build-id" #> Build.item env e
