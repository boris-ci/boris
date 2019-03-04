{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Project (
    byId
  , byName
  , list

  , NewProjectError (..)
  , new
  ) where

import           Boris.Core.Data.Keyed
import           Boris.Core.Data.Project
import           Boris.Core.Data.Repository
import qualified Boris.Http.Db.Project as ProjectDb
import           Boris.Prelude

import           Traction.Sql (Unique (..))
import           Traction.Control (Db)


byName :: ProjectName -> Db (Maybe (Keyed ProjectId Project))
byName =
  ProjectDb.byName

byId :: ProjectId -> Db (Maybe (Keyed ProjectId Project))
byId =
  ProjectDb.byId

list :: Db [Keyed ProjectId Project]
list =
  ProjectDb.list

data NewProjectError =
    NewProjectAlreadyExists ProjectName
  | NewProjectInvalidNameError ProjectName
  | NewProjectInvalidRepositoryError ProjectName Repository
    deriving (Eq, Ord, Show)

new :: ProjectName -> Repository -> EitherT NewProjectError Db ProjectId
new project repository = do
  unless (isValidProjectName project) $
    left $ NewProjectInvalidNameError project
  unless (isValidRepository repository) $
    left $ NewProjectInvalidRepositoryError project repository
  newEitherT $ ProjectDb.insert project repository >>= \u -> case u of
    Unique i ->
      pure . Right $ i
    Duplicate _ _ ->
      pure . Left $ NewProjectAlreadyExists project
