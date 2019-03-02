{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
module Boris.Http.Data (
    ErrorId (..)
  , newErrorId
  , GithubClient (..)
  , GithubSecret (..)
  , GithubUser (..)
  , GithubOrganisation (..)
  , GithubCode (..)
  , GithubOAuth (..)
  , GithubId (..)
  , GithubLogin (..)
  , GithubName (..)
  , GithubEmail (..)
  , OwnedBy (..)
  , BorisSystemUser (..)
  , Identified (..)
  , UserId (..)
  , User (..)
  , AuthenticatedUser (..)
  , Session (..)
  , SessionId (..)
  , Authenticated (..)
  , AuthenticatedBy (..)
  , newSessionToken
  , Permission (..)
  , permissionToInt
  , permissionFromInt
  , EntityId (..)
  , EntityType (..)
  , entityTypeToInt
  , entityTypeFromInt
  , IdentityId (..)
  , IdentityType (..)
  , identityTypeToInt
  , identityTypeFromInt
  , identityTypeToEntityType
  ) where

import           Boris.Prelude

import           Control.Monad.IO.Class (MonadIO (..))

import qualified Crypto.Random.Entropy as Entropy

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import           System.IO (IO)

newtype ErrorId =
  ErrorId {
    errorId :: Text
  } deriving (Eq, Ord, Show)

newErrorId :: IO ErrorId
newErrorId =
  ErrorId <$>
    Text.decodeUtf8 . Base16.encode <$> Entropy.getEntropy 16

newtype GithubClient =
  GithubClient {
      githubClient :: Text
    } deriving (Eq, Ord, Show)

newtype GithubSecret =
  GithubSecret {
      githubSecret :: Text
    } deriving (Eq, Ord, Show)

newtype GithubCode =
  GithubCode {
      githubCode :: Text
    } deriving (Eq, Ord, Show)

newtype GithubOAuth =
  GithubOAuth {
      githubOAuth :: ByteString
    } deriving (Eq, Ord, Show)

newtype GithubId =
  GithubId {
      githubId :: Int
    } deriving (Eq, Ord, Show)

newtype GithubLogin =
  GithubLogin {
      githubLogin :: Text
    } deriving (Eq, Ord, Show)

newtype GithubName =
  GithubName {
      githubName :: Text
    } deriving (Eq, Ord, Show)

newtype GithubEmail =
  GithubEmail {
      githubEmail :: Text
    } deriving (Eq, Ord, Show)

data GithubUser =
  GithubUser {
      githubUserId :: GithubId
    , githubUserLogin :: GithubLogin
    , githubUserName :: Maybe GithubName
    , githubUserEmail :: Maybe GithubEmail
    } deriving (Eq, Show)

data GithubOrganisation =
  GithubOrganisation {
      githubOrganisationId :: GithubId
    , githubOrganisationName :: GithubName
    } deriving (Eq, Show)

newtype UserId =
  UserId {
      getUserId :: Int64
    } deriving (Eq, Ord, Show)

data Identified a =
  Identified {
      userIdOf :: UserId
    , userOf :: a
    } deriving (Eq, Show, Functor)

data User =
    UserFromGithub GithubUser
  | UserFormBoris BorisSystemUser
    deriving (Eq, Show)

data OwnedBy =
    OwnedByGithubUser GithubLogin
  | OwnedByGithubOrganisation GithubName
  | OwnedByBoris BorisSystemUser
    deriving (Eq, Show)

data BorisSystemUser =
    BorisSystemUser
    deriving (Eq, Show)

data BorisServiceAccount =
    BorisServiceAccount
    deriving (Eq, Show)

data AuthenticatedUser =
  AuthenticatedUser {
      authenticatedUser :: Identified GithubUser
    , authenticatedSession :: Session
    } deriving (Eq, Show)

newtype SessionId =
  SessionId {
      getSessionId :: Text
    } deriving (Eq, Ord, Show)

data Session =
  Session {
      sessionIdentifier :: SessionId
    , sessionOAuth :: GithubOAuth
    } deriving (Eq, Ord, Show)

data Authenticated =
    Authenticated Session (Identified GithubUser)
  | AuthenticatedNone (Identified BorisSystemUser)
  | NotAuthenticated
  | WasAuthenticated SessionId
    deriving (Eq, Show)

data AuthenticatedBy =
    AuthenticatedByGithub Session (Identified GithubUser)
  | AuthenticatedByDesign (Identified BorisSystemUser)
-- TODO: This will need to be a thing once agents are involved...
--  | AuthenticatedByServiceAccount UserId BorisServiceAccount
    deriving (Eq, Show)


newSessionToken :: MonadIO m => m SessionId
newSessionToken =
  liftIO $
    SessionId . Text.decodeUtf8 . Base16.encode <$> Entropy.getEntropy 16

data Permission =
    Admin
  | Write
  | Read
    deriving (Eq, Ord, Show, Enum, Bounded)

permissionToInt :: Permission -> Int
permissionToInt p =
  case p of
    Admin ->
      0
    Write ->
      1
    Read ->
      2

permissionFromInt :: Int -> Maybe Permission
permissionFromInt n =
  case n of
    0 ->
      Just Admin
    1 ->
      Just Write
    2 ->
      Just Read
    _ ->
      Nothing

newtype EntityId =
  EntityId {
      getEntityId :: Int64
    } deriving (Eq, Ord, Show)

data EntityType =
    IsUserEntity
  | IsServiceEntity
  | IsTeamEntity
    deriving (Eq, Ord, Show, Enum, Bounded)

entityTypeToInt :: EntityType -> Int
entityTypeToInt i =
  case i of
    IsUserEntity ->
      0
    IsServiceEntity ->
      1
    IsTeamEntity ->
      2

entityTypeFromInt :: Int -> Maybe EntityType
entityTypeFromInt i =
  case i of
    0 ->
      Just IsUserEntity
    1 ->
      Just IsServiceEntity
    2 ->
      Just IsTeamEntity
    _ ->
      Nothing

newtype IdentityId =
  IdentityId {
      getIdentityId :: Int64
    } deriving (Eq, Ord, Show)

identityTypeToEntityType :: IdentityType -> EntityType
identityTypeToEntityType t =
  case t of
    IsUserAccount ->
      IsUserEntity
    IsServiceAccount ->
      IsServiceEntity

data IdentityType =
    IsUserAccount
  | IsServiceAccount
    deriving (Eq, Ord, Show, Enum, Bounded)

identityTypeToInt :: IdentityType -> Int
identityTypeToInt i =
  case i of
    IsUserAccount ->
      0
    IsServiceAccount ->
      1

identityTypeFromInt :: Int -> Maybe IdentityType
identityTypeFromInt i =
  case i of
    0 ->
      Just IsUserAccount
    1 ->
      Just IsServiceAccount
    _ ->
      Nothing
