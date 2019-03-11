{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Discover (
    complete

  , CompleteError (..)
  , renderCompleteError

  , discover
  , tryDiscover
  ) where


import qualified Data.List as List

import           Boris.Core.Data.Build
import           Boris.Core.Data.Discover
import           Boris.Core.Data.Keyed
import           Boris.Core.Data.Instance
import           Boris.Core.Data.Run
import           Boris.Core.Data.Project
import           Boris.Core.Data.Tenant
import           Boris.Http.Data
import qualified Boris.Http.Api.Build as Build
import qualified Boris.Http.Db.Build as BuildDb
import qualified Boris.Http.Db.Run as RunDb
import qualified Boris.Http.Db.Queue as QueueDb
import qualified Boris.Http.Db.Discover as DiscoverDb
import qualified Boris.Http.Db.Project as ProjectDb
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

complete :: DiscoverId -> [DiscoverInstance] -> Db (Maybe [Keyed BuildId Build])
complete i discovers = do
  mdiscover <- DiscoverDb.byId i
  for mdiscover $ \discover -> do
    fmap join . for discovers $ \(DiscoverInstance build ref commit) -> do
      current <- BuildDb.built (keyOf . discoverProject . valueOf $ discover) commit
      already <- DiscoverDb.discovered (keyOf . discoverProject . valueOf $ discover) commit
      if List.elem build current || List.elem build already then
        pure []
      else do
        void $ DiscoverDb.add i build commit
        -- FIX not sure about ref - semantics way less clear than
        --     i remember them being :/
        fmap pure $ Build.submitWith (discoverProject . valueOf $ discover) build (Just ref)

discover :: Keyed ProjectId Project -> Db (Keyed DiscoverId Discover)
discover project = do
  run <- RunDb.insert IsDiscover (keyOf project)
  QueueDb.insert run IsDiscover
  i <- DiscoverDb.insert run
  pure $ Keyed i (Discover project Nothing Nothing Nothing Nothing Nothing)

tryDiscover :: ProjectName -> Db (Maybe (Keyed DiscoverId Discover))
tryDiscover name = do
  mproject <- ProjectDb.byName name
  for mproject $ \project -> do
    discover project
