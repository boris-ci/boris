{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Discover (
    complete
  , discover

  , CompleteError (..)
  , renderCompleteError
  ) where


import qualified Data.List as List

import           Boris.Core.Data.Build
import           Boris.Core.Data.Instance
import           Boris.Core.Data.Project
import           Boris.Core.Data.Tenant
import           Boris.Http.Data
import qualified Boris.Http.Api.Project as Project
import qualified Boris.Http.Db.Build as BuildDb
import qualified Boris.Http.Db.Discover as DiscoverDb
import qualified Boris.Http.Db.Tick as TickDb

import           P

import           System.IO (IO)

import           Traction.Control (DbPool, DbError)
import qualified Traction.Control as Traction

import           X.Control.Monad.Trans.Either (EitherT)


data CompleteError =
    CompleteDbError DbError

renderCompleteError :: CompleteError -> Text
renderCompleteError err =
  case err of
   CompleteDbError e ->
      mconcat ["Complete error via db: ", Traction.renderDbError e]

complete :: DbPool -> BuildId -> Project -> [DiscoverInstance] -> EitherT CompleteError IO ()
complete pool buildid project discovers = do
  -- FIX ref should be handled
  for_ discovers $ \(DiscoverInstance build _ref commit) -> do
    current <- firstT CompleteDbError . Traction.runDb pool $
      BuildDb.getProjectCommitSeen project commit
    already <- firstT CompleteDbError . Traction.runDb pool $
      DiscoverDb.getProjectCommitDiscovered project commit
    if List.elem build current || List.elem build already
      then pure ()
      else
        firstT CompleteDbError . Traction.runDb pool $ do
          DiscoverDb.addProjectCommitDiscovered buildid build commit
          -- FIX should this just call Build.submit? Permissions will be wierd.
          newId <- TickDb.tick
          BuildDb.register project build newId

-- FIX MTH error type
discover :: DbPool -> Tenant -> AuthenticatedBy -> Project -> EitherT Text IO (Maybe BuildId)
discover pool tenant authenticated project = do
  r <- firstT Traction.renderDbError $
    Project.pick pool tenant authenticated project
  case r of
    Nothing ->
      pure Nothing
    Just _repository -> do
      i <- firstT Traction.renderDbError . Traction.runDb pool $
        TickDb.tick
      firstT Traction.renderDbError . Traction.runDb pool $
        DiscoverDb.discover i project
      pure (Just i)
