{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Discover (
    complete

  , CompleteError (..)
  , renderCompleteError
  ) where


import qualified Data.List as List

import           Boris.Core.Data
import qualified Boris.Http.Api.Project as Project
import           Boris.Http.Boot

import qualified Boris.Http.Service as Service
import qualified Boris.Http.Store.Api as Store
import           Boris.Http.Store.Data
import qualified Boris.Http.Store.Error as Store
import           Boris.Queue (Request (..), RequestBuild (..))
import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)


data CompleteError =
    CompleteStoreError Store.StoreError
  | CompleteConfigError Project.ConfigError
  | CompleteRegisterError Store.RegisterError
  | CompleteServiceError Service.ServiceError

renderCompleteError :: CompleteError -> Text
renderCompleteError err =
  case err of
   CompleteStoreError e ->
      mconcat ["Discover error via store backend: ", Store.renderStoreError e]
   CompleteRegisterError e ->
      mconcat ["Discoverregistration error via store backend: ", Store.renderRegisterError e]
   CompleteConfigError e ->
      mconcat ["Discover project configuration error: ", Project.renderConfigError e]
   CompleteServiceError e ->
      mconcat ["Discover service error: ", Service.renderServiceError e]

complete :: Store -> BuildService -> ProjectMode -> BuildId -> Project -> [DiscoverInstance] -> EitherT CompleteError IO ()
complete store buildx projectx buildid project discovers = do
  for_ discovers $ \(DiscoverInstance build ref commit) -> do
    current <- firstT CompleteStoreError $
      Store.getProjectCommitSeen store project commit
    already <- firstT CompleteStoreError $
      Store.getProjectCommitDiscovered store project commit
    if List.elem build current || List.elem build already
      then pure ()
      else do
        firstT CompleteStoreError $
          Store.addProjectCommitDiscovered store buildid project commit ref build
        newId <- firstT CompleteStoreError $
          Store.tick store
        firstT CompleteRegisterError $
          Store.register store project build newId
        repository' <- firstT CompleteConfigError $
          Project.pick projectx project
        firstT CompleteServiceError . for_ repository' $ \repository ->
          Service.put buildx
            (RequestBuild' $ RequestBuild newId project repository build (Just ref))
