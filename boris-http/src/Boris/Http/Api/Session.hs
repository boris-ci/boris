{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Session (
    authenticate
  , check

  , AuthenticationError (..)
  , renderAuthenticationError
  ) where


import           Boris.Core.Data
import           Boris.Http.Boot
import           Boris.Http.Data
import qualified Boris.Http.Api.Project as Project
import qualified Boris.Http.Service as Service
import           Boris.Http.Store.Data
import qualified Boris.Http.Store.Api as Store
import qualified Boris.Http.Store.Error as Store
import           Boris.Queue (Request (..), RequestBuild (..))

import           Control.Monad.IO.Class (MonadIO (..))

import qualified Crypto.Random.Entropy as Entropy

import           Data.Aeson (object, (.=), (.:))
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import           Data.Default (def)
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified GitHub.Auth as Github
import qualified GitHub.Data as Github
import qualified GitHub.Endpoints.Users as Github

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as Client
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, newEitherT)


data AuthenticationError =
    AuthenticationStoreError Store.StoreError
  | AuthenticationGithubDecodeError Text
  | AuthenticationGithubError Github.Error
    deriving (Show)

renderAuthenticationError :: AuthenticationError -> Text
renderAuthenticationError err =
  case err of
   AuthenticationStoreError e ->
      mconcat ["Authentication error via store backend: ", Store.renderStoreError e]
   AuthenticationGithubDecodeError e ->
      mconcat ["Authentication error decoding github response: ", e]
   AuthenticationGithubError e ->
      mconcat ["Authentication error contacting github: ", Text.pack . show $ e]

check :: Store -> Client.Manager -> GithubClient -> GithubSecret -> SessionId -> EitherT AuthenticationError IO (Maybe UserId)
check store _manager _client _secret sessionId = do
  a <- firstT AuthenticationStoreError $
    Store.getSessionUser store sessionId
  case a of
    Nothing ->
      pure Nothing
    Just _ -> do
      firstT AuthenticationStoreError $
        Store.tickSession store sessionId
      pure a

authenticate :: Store -> Client.Manager -> GithubClient -> GithubSecret -> GithubCode -> EitherT AuthenticationError IO Session
authenticate store manager client secret code = do
  response <- liftIO $ Client.httpLbs def {
      Client.host = "github.com"
    , Client.port = 443
    , Client.secure = True
    , Client.checkStatus = \_ _ _ -> Nothing
    , Client.redirectCount = 0
    , Client.method = HTTP.methodPost
    , Client.requestHeaders = [
        ("Content-Type", "application/json")
      , ("Accept", "application/json")
      ]
    , Client.path = "login/oauth/access_token"
    , Client.requestBody = Client.RequestBodyLBS . Aeson.encode $
        object [
            "client_id" .= githubClient client
          , "client_secret" .= githubSecret secret
          , "code" .= githubCode code
          ]
    } manager
  access <- firstT AuthenticationGithubDecodeError . newEitherT . pure $
    fmap getAccessToken . either (Left . Text.pack) Right . Aeson.eitherDecode . Client.responseBody $ response

  user <- firstT AuthenticationGithubError . newEitherT $
    Github.userInfoCurrent' $ Github.OAuth access

  let
    githubUser =
      GithubUser
        (GithubId . Github.untagId . Github.userId $ user)
        (GithubLogin . Github.untagName . Github.userLogin $ user)
        (fmap GithubName . Github.userName $ user)
        (fmap GithubEmail . Github.userEmail $ user)
  mu <- firstT AuthenticationStoreError $
    Store.userByGithubId store (githubUserId githubUser)
  u <- case mu of
    Nothing ->
      firstT AuthenticationStoreError $
        Store.addUser store githubUser
    Just u ->
      if githubUser == userGithub u then
        pure u
      else do
        firstT AuthenticationStoreError $
          Store.updateUser store (u { userGithub = githubUser})
        pure u
  s <- newSessionToken
  let
    session = Session s (GithubOAuth access)

  firstT AuthenticationStoreError $
    Store.newSession store session u
  pure session

newtype AccessToken =
  AccessToken {
      getAccessToken :: ByteString
    } deriving (Eq, Ord, Show)

instance Aeson.FromJSON AccessToken where
  parseJSON =
    Aeson.withObject "AccessToken" $ \o ->
      AccessToken . Text.encodeUtf8 <$> o .: "access_token"

newSessionToken :: MonadIO m => m SessionId
newSessionToken =
  liftIO $
    SessionId . Text.decodeUtf8 . Base16.encode <$> Entropy.getEntropy 16
