{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Build (
    byProjectId
  , byId
  , byBuildName
  , queued


  , submit
  , heartbeat
  , acknowledge
  , cancel
  , byCommit
  , byProject
  , logOf
  , avow
  , complete
  , BuildError (..)
  , renderBuildError
  ) where


import           Control.Monad.IO.Class (MonadIO (..))

import qualified Data.Text as Text
import qualified Data.Time as Time

import           Boris.Core.Data.Build
import           Boris.Core.Data.Keyed
import           Boris.Core.Data.Log
import           Boris.Core.Data.Project
import           Boris.Core.Data.Run
import           Boris.Core.Data.Tenant
import qualified Boris.Http.Api.Project as Project
import           Boris.Http.Data
import qualified Boris.Http.Db.Build as BuildDb
import qualified Boris.Http.Db.Project as ProjectDb
import qualified Boris.Http.Db.Run as RunDb
import qualified Boris.Http.Db.Query as Query
import           Boris.Prelude

import           System.IO (IO)

import           Traction.Control (Db, DbPool, DbError)
import qualified Traction.Control as Traction


data BuildError =
    BuildDbError Traction.DbError

renderBuildError :: BuildError -> Text
renderBuildError err =
  case err of
    BuildDbError e ->
      mconcat ["Build error via db: ", Traction.renderDbError e]

byProjectId :: ProjectId -> Db [Keyed BuildId Build]
byProjectId =
  BuildDb.byProjectId

byId :: BuildId -> Db (Maybe (Keyed BuildId Build))
byId =
  BuildDb.byId

byBuildName :: ProjectName -> BuildName -> Db BuildTree
byBuildName project build =
  BuildDb.refTree project build

queued :: ProjectName -> BuildName -> Db [Keyed BuildId Build]
queued project build =
  BuildDb.isQueued project build

submit :: ProjectName -> BuildName -> Maybe Ref -> Db (Maybe (Keyed BuildId Build))
submit p build ref = do
  mproject <- ProjectDb.byName p
  for mproject $ \project -> do
    let
      normalised = with ref $ \rr ->
        if Text.isPrefixOf "refs/" . renderRef $ rr then rr else Ref . ((<>) "refs/heads/") . renderRef $ rr
    run <- RunDb.insert IsBuild (keyOf project)
    i <- BuildDb.insert run build normalised
    pure $ Keyed i (Build project build ref Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

heartbeat :: DbPool -> BuildId -> EitherT DbError IO BuildCancelled
heartbeat pool  buildId =
  error "todo"
  {--
  Traction.runDb pool $
    Query.heartbeat buildId
--}

acknowledge :: DbPool -> BuildId -> EitherT DbError IO Acknowledge
acknowledge pool buildId =
  error "todo"
  {--
  Traction.runDb pool $
    Query.acknowledge buildId
--}

cancel :: DbPool -> BuildId -> EitherT DbError IO (Maybe ())
cancel pool i =
  error "Todo"
  {--
  Traction.runDb pool $ do
    d <- Query.fetch i
    for d $ \_ ->
        Query.cancel i
--}
byCommit :: DbPool -> ProjectName -> Commit -> EitherT DbError IO [BuildId]
byCommit pool project commit =
  error "todo"
  {--
  Traction.runDb pool $
    Query.getProjectCommitBuildIds project commit

--}

byProject :: DbPool -> ProjectName -> EitherT DbError IO [BuildName]
byProject pool project =
  error "todo"
  {--
  Traction.runDb pool $
    Query.getProjects project
--}

logOf :: DbPool -> BuildId -> EitherT DbError IO (Maybe LogData)
logOf pool i =
  error "todo"
  {--
  Traction.runDb pool $ do
    d <- Query.fetch i
    for d $ \_ ->
      Query.fetchLogData i
--}

avow :: DbPool -> BuildId -> Ref -> Commit -> EitherT DbError IO ()
avow pool i ref commit =
  error "todo"
  {--
  Traction.runDb pool $
    Query.index i ref commit
--}
complete :: DbPool -> BuildId -> BuildResult -> EitherT DbError IO ()
complete pool i result =
  error "Todo"
  {--
  void . Traction.runDb pool $
    Query.complete i result
--}
