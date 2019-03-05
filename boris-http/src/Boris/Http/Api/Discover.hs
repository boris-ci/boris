{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Discover (
    complete

  , CompleteError (..)
  , renderCompleteError

  , discover
  ) where


import qualified Data.List as List

import           Boris.Core.Data.Build
import           Boris.Core.Data.Discover
import           Boris.Core.Data.Keyed
import           Boris.Core.Data.Instance
import           Boris.Core.Data.Project
import           Boris.Core.Data.Tenant
import           Boris.Http.Data
import qualified Boris.Http.Db.Query as Query
import           Boris.Prelude

import           System.IO (IO)

import           Traction.Control (Db, DbPool, DbError)
import qualified Traction.Control as Traction


data CompleteError =
    CompleteDbError DbError

renderCompleteError :: CompleteError -> Text
renderCompleteError err =
  case err of
   CompleteDbError e ->
      mconcat ["Complete error via db: ", Traction.renderDbError e]

complete :: DbPool -> BuildId -> ProjectName -> [DiscoverInstance] -> EitherT CompleteError IO ()
complete pool buildid project discovers = do
  -- FIX ref should be handled
  error "todo"
  {--
  for_ discovers $ \(DiscoverInstance build _ref commit) -> do
    current <- firstT CompleteDbError . Traction.runDb pool $
      Query.getProjectCommitSeen project commit
    already <- firstT CompleteDbError . Traction.runDb pool $
      Query.getProjectCommitDiscovered project commit
    if List.elem build current || List.elem build already
      then pure ()
      else
        firstT CompleteDbError . Traction.runDb pool $ do
          Query.addProjectCommitDiscovered buildid build commit
          -- FIX should this just call Build.submit? Permissions will be wierd.
          newId <- Query.tick
          error "todo"
--          Query.register project build newId
--}

discover :: ProjectName -> Db (Maybe (Keyed DiscoverId Discover))
discover project = do
  error "todo"
  {--
  r <- firstT Traction.renderDbError $
    pick pool tenant authenticated project
  case r of
    Nothing ->
      pure Nothing
    Just _repository -> do
      i <- firstT Traction.renderDbError . Traction.runDb pool $
        Query.tick
      firstT Traction.renderDbError . Traction.runDb pool $
        Query.discover i project
      pure (Just i)
--}
