{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
module Boris.Http.Data (
    ErrorId (..)
  , newErrorId
  , GithubClient (..)
  , GithubSecret (..)
  , GithubUser (..)
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
  ) where

import           Control.Monad.IO.Class (MonadIO (..))

import qualified Crypto.Random.Entropy as Entropy

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import           P

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
      getUserId :: Int
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
    OwnedByGithubUser GithubUser
  | OwnedByGithubOrganisation GithubOrganisation
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
