{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Build (
    byProjectId
  , byId
  , byBuildName
  , queued
  , next
  , submit
  , submitWith
  , byCommit
  , byProject
  , logOf
  , avow
  , complete
  , cancel
  , heartbeat
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
import qualified Boris.Http.Db.Queue as QueueDb
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

next :: Db (Maybe (Keyed BuildId Build))
next =
  BuildDb.next

submit :: ProjectName -> BuildName -> Maybe Ref -> Db (Maybe (Keyed BuildId Build))
submit p build ref = do
  mproject <- ProjectDb.byName p
  for mproject $ \project ->
    submitWith project build ref

submitWith :: (Keyed ProjectId Project) -> BuildName -> Maybe Ref -> Db (Keyed BuildId Build)
submitWith project build ref = do
  let
    normalised = with ref $ \rr ->
      if Text.isPrefixOf "refs/" . renderRef $ rr then rr else Ref . ((<>) "refs/heads/") . renderRef $ rr
  run <- RunDb.insert IsBuild (keyOf project)
  QueueDb.insert run IsBuild
  i <- BuildDb.insert run build normalised
  pure $ Keyed i (Build project build ref Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

heartbeat :: BuildId -> Db BuildCancelled
heartbeat b =
  RunDb.heartbeat (RunId . getBuildId $ b)

cancel :: BuildId -> Db Bool
cancel b = do
  RunDb.cancel (RunId . getBuildId $ b)

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

avow :: BuildId -> Ref -> Commit -> Db ()
avow i ref commit =
  BuildDb.index i ref commit

complete :: BuildId -> BuildResult -> Db ()
complete i result =
  void $ BuildDb.complete i result
