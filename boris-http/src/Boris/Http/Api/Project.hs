{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Project (
    picker
  , pick
  , list
  , discover
  , new
  ) where

import           Boris.Core.Data
import           Boris.Http.Data
import qualified Boris.Http.Db.Query as Query

import qualified Data.List as List

import           P

import           System.IO (IO)

import           Traction.Control (DbPool, DbError)
import qualified Traction.Control as Traction

import           X.Control.Monad.Trans.Either (EitherT)

picker :: Project -> [Definition] -> Maybe Repository
picker project =
  fmap definitionRepository . List.find ((==) project . definitionProject)

pick :: DbPool -> Settings -> AuthenticatedBy -> Project -> EitherT DbError IO (Maybe Repository)
pick pool settings authenticated project =
  Traction.runDb pool $ case settings of
    SingleTenantSettings ->
      case authenticated of
        AuthenticatedByDesign ->
          picker project <$> Query.getAllProjects
        AuthenticatedByOAuth _session user ->
           picker project <$> Query.getAccountProjects (userId user)
    MultiTenantSettings ->
      case authenticated of
        AuthenticatedByDesign ->
          pure Nothing
        AuthenticatedByOAuth _session user ->
          picker project <$> Query.getAccountProjects (userId user)

list :: DbPool -> Settings -> AuthenticatedBy -> EitherT DbError IO [Project]
list pool settings authenticated =
  (fmap . fmap) definitionProject . Traction.runDb pool $ case settings of
    SingleTenantSettings ->
      case authenticated of
        AuthenticatedByDesign ->
          Query.getAllProjects
        AuthenticatedByOAuth _session user ->
          Query.getAccountProjects (userId user)
    MultiTenantSettings ->
      case authenticated of
        AuthenticatedByDesign ->
          pure []
        AuthenticatedByOAuth _session user ->
          Query.getAccountProjects (userId user)

-- FIX MTH error type
discover :: DbPool -> Settings -> AuthenticatedBy -> Project -> EitherT Text IO (Maybe BuildId)
discover pool settings authenticated project = do
  r <- firstT Traction.renderDbError $
    pick pool settings authenticated project
  case r of
    Nothing ->
      pure Nothing
    Just _repository -> do
      i <- firstT Traction.renderDbError . Traction.runDb pool $
        Query.tick
      firstT Traction.renderDbError . Traction.runDb pool $
        Query.discover i project
      pure (Just i)

new :: DbPool -> Settings -> AuthenticatedBy -> Project -> Repository -> EitherT DbError IO ()
new pool settings authenticated project repository =
  error "todo"
