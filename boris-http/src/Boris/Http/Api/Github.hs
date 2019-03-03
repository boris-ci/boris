{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Github (
    importRepositories
  , ImportError (..)
  , renderImportError
  ) where


import           Boris.Core.Data.Project
import           Boris.Core.Data.Repository
import           Boris.Http.Data
import qualified Boris.Http.Db.Query as Query
import           Boris.Prelude

import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Aeson (FromJSON (..), withObject, (.:))
--import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector

import qualified GitHub.Auth as Github
import qualified GitHub.Data as Github
import qualified GitHub.Request as Github
import qualified GitHub.Endpoints.Repos as Github

import           System.IO (IO)

import           Traction.Control (DbPool, DbError)
import qualified Traction.Control as Traction


data ImportError =
    ImportDbError DbError
  | ImportGithubError Github.Error
    deriving (Show)

renderImportError :: ImportError -> Text
renderImportError err =
  case err of
   ImportDbError e ->
      mconcat ["Import error via db: ", Traction.renderDbError e]
   ImportGithubError e ->
      mconcat ["Import error contacting github: ", Text.pack . show $ e]

importRepositories :: DbPool -> Session -> (Identified GithubUser) -> EitherT ImportError IO ()
importRepositories pool session login = do
  error "todo"
  {--
  repositories <- fmap Vector.toList . firstT ImportGithubError . newEitherT $
    Github.currentUserRepos (Github.OAuth . githubOAuth . sessionOAuth $ session) Github.RepoPublicityAll

  for_ (repositories) $ \r -> do
    let
      project = Project . Github.untagName . Github.repoName $ r
      sowner = Github.repoOwner $ r
      name = Github.untagName . Github.simpleOwnerLogin $ sowner
      owner = case Github.simpleOwnerType sowner of
        Github.OwnerUser ->
          OwnedByGithubUser (GithubLogin name)
        Github.OwnerOrganization ->
          OwnedByGithubOrganisation (GithubName name)
      mrepository = fmap (Repository . Github.getUrl) . Github.repoSshUrl $ r
    case mrepository of
      Nothing ->
        pure ()
      Just repository -> do
        liftIO . Text.putStrLn . mconcat $ [
            "Importing ", name, "/", renderProject project
          ]
        permission <- mapEitherT liftIO . firstT ImportGithubError $
          permissionOn (Github.OAuth . githubOAuth . sessionOAuth $ session) (Github.simpleOwnerLogin sowner) (Github.repoName r) (githubUserLogin . userOf $ login)
        firstT ImportDbError . Traction.runDb pool $ do
          pid <- Query.importProject owner project repository
          case permission of
            GithubPermissionAdmin ->
              Query.linkProject pid (userIdOf login) Admin
            GithubPermissionWrite ->
              Query.linkProject pid (userIdOf login) Write
            GithubPermissionRead ->
              Query.linkProject pid (userIdOf login) Read
            GithubPermissionNone ->
              pure ()
        liftIO . Text.putStrLn . mconcat $ [
            "Imported ", name, "/", renderProject project
          ]

 --}
data GithubPermission =
    GithubPermissionAdmin
  | GithubPermissionWrite
  | GithubPermissionRead
  | GithubPermissionNone
    deriving (Eq, Ord, Show, Enum, Bounded)

instance FromJSON GithubPermission where
  parseJSON =
    withObject "GithubPermission" $ \o ->
      o .: "permission" >>= \p -> case (p :: Text) of
        "admin" ->
          pure GithubPermissionAdmin
        "write" ->
          pure GithubPermissionWrite
        "read" ->
          pure GithubPermissionRead
        _ ->
          pure GithubPermissionNone

permissionOn :: Github.Auth -> Github.Name Github.Owner -> Github.Name Github.Repo -> GithubLogin -> EitherT Github.Error IO GithubPermission
permissionOn auth user repo login =
  newEitherT . Github.executeRequestMaybe (Just auth) $
    Github.query ["repos", Github.toPathPart user, Github.toPathPart repo, "collaborators", githubLogin login, "permission"] []

{--
collaboratorsOn :: Github.Auth -> Github.Name Github.Owner -> Github.Name Github.Repo -> EitherT Github.Error IO [PermissableUser]
collaboratorsOn auth user repo =
  fmap Vector.toList . newEitherT . Github.executeRequestMaybe (Just auth) $
    Github.pagedQuery ["repos", Github.toPathPart user, Github.toPathPart repo, "collaborators"] [] Github.FetchAll

data PermissableUser =
  PermissableUser {
      permissableSimpleUser :: Github.SimpleUser
    , permissableAdmin :: Bool
    , permissablePush :: Bool
    , permissablePull :: Bool
    } deriving (Eq, Show)

instance FromJSON PermissableUser where
  parseJSON =
    withObject "PermissableUser" $ \o ->
      PermissableUser
        <$> parseJSON (Aeson.Object o)
        <*> (o .: "permissions" >>= withObject "Permissions" (\oo -> oo .: "admin"))
        <*> (o .: "permissions" >>= withObject "Permissions" (\oo -> oo .: "push"))
        <*> (o .: "permissions" >>= withObject "Permissions" (\oo -> oo .: "pull"))

--}
