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
import           Boris.Http.Data
import qualified Boris.Http.Service as Service
import qualified Boris.Http.Db.Query as Query
import           Boris.Queue (Request (..), RequestBuild (..))

import           P

import           System.IO (IO)

import           Traction.Control (DbPool, DbError)
import qualified Traction.Control as Traction

import           X.Control.Monad.Trans.Either (EitherT)


data CompleteError =
    CompleteDbError DbError
  | CompleteServiceError Service.ServiceError

renderCompleteError :: CompleteError -> Text
renderCompleteError err =
  case err of
   CompleteDbError e ->
      mconcat ["Discover error via db: ", Traction.renderDbError e]
   CompleteServiceError e ->
      mconcat ["Discover service error: ", Service.renderServiceError e]

complete :: DbPool -> Settings -> AuthenticatedBy -> BuildService -> ProjectMode -> BuildId -> Project -> [DiscoverInstance] -> EitherT CompleteError IO ()
complete pool settings authenticated  buildx projectx buildid project discovers = do
  for_ discovers $ \(DiscoverInstance build ref commit) -> do
    current <- firstT CompleteDbError . Traction.runDb pool $
      Query.getProjectCommitSeen project commit
    already <- firstT CompleteDbError . Traction.runDb pool $
      Query.getProjectCommitDiscovered project commit
    if List.elem build current || List.elem build already
      then pure ()
      else do
        firstT CompleteDbError . Traction.runDb pool $
          Query.addProjectCommitDiscovered buildid build commit
        newId <- firstT CompleteDbError . Traction.runDb pool $
          Query.tick
        firstT CompleteDbError . Traction.runDb pool $
          Query.register project build newId
        repository' <- firstT CompleteDbError $
          Project.pick pool settings authenticated project
        firstT CompleteServiceError . for_ repository' $ \repository ->
          Service.put buildx
            (RequestBuild' $ RequestBuild newId project repository build (Just ref))
