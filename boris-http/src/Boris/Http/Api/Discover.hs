{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Discover (
    complete

  , CompleteError (..)
  , renderCompleteError
  ) where


import qualified Data.List as List

import           Boris.Core.Data
import qualified Boris.Http.Db.Query as Query

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
          Query.register project build newId
