{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Session (
    authenticate
  , check

  , AuthenticationError (..)
  , renderAuthenticationError
  ) where


import           Boris.Http.Data
import qualified Boris.Http.Db.Account as AccountDb
import qualified Boris.Http.Db.Session as SessionDb

import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Aeson (object, (.:), (.=))
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import           Data.Default (def)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified GitHub.Auth as Github
import qualified GitHub.Data as Github
import qualified GitHub.Endpoints.Users as Github

import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as HTTP

import           P

import           System.IO (IO)
import qualified System.IO as IO


import           Traction.Control (DbPool, DbError)
import qualified Traction.Control as Traction

import           X.Control.Monad.Trans.Either (EitherT, newEitherT)


data AuthenticationError =
    AuthenticationDbError DbError
  | AuthenticationGithubDecodeError Text
  | AuthenticationGithubError Github.Error
    deriving (Show)

renderAuthenticationError :: AuthenticationError -> Text
renderAuthenticationError err =
  case err of
   AuthenticationDbError e ->
      mconcat ["Authentication error via db: ", Traction.renderDbError e]
   AuthenticationGithubDecodeError e ->
      mconcat ["Authentication error decoding github response: ", e]
   AuthenticationGithubError e ->
      mconcat ["Authentication error contacting github: ", Text.pack . show $ e]

check :: DbPool -> Client.Manager -> GithubClient -> GithubSecret -> SessionId -> EitherT AuthenticationError IO (Maybe AuthenticatedUser)
check pool _manager _client _secret sessionId = do
  a <- firstT AuthenticationDbError . Traction.runDb pool $
    SessionDb.getSession sessionId
  case a of
    Nothing ->
      pure Nothing
    Just _ -> do
      firstT AuthenticationDbError . Traction.runDb pool $
        SessionDb.tickSession sessionId
      pure a

authenticate :: DbPool -> Client.Manager -> GithubClient -> GithubSecret -> GithubCode -> EitherT AuthenticationError IO Session
authenticate pool manager client secret code = do
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
    either (Left . Text.pack) Right . Aeson.eitherDecode . Client.responseBody $ response

  user <- firstT AuthenticationGithubError . newEitherT $
    Github.userInfoCurrent' $ Github.OAuth (getAccessToken access)

  let
    githubUser =
      GithubUser
        (GithubId . Github.untagId . Github.userId $ user)
        (GithubLogin . Github.untagName . Github.userLogin $ user)
        (fmap GithubName . Github.userName $ user)
        (fmap GithubEmail . Github.userEmail $ user)
  mu <- firstT AuthenticationDbError . Traction.runDb pool $
    AccountDb.userByGithubId (githubUserId githubUser)
  u <- case mu of
    Nothing ->
      firstT AuthenticationDbError . Traction.runDb pool $
        AccountDb.addUser githubUser
    Just u ->
      if githubUser == userOf u then
        pure u
      else do
        firstT AuthenticationDbError . Traction.runDb pool $
          AccountDb.updateUser (u { userOf = githubUser})
        pure u
  s <- newSessionToken
  let
    session = Session s (GithubOAuth $ getAccessToken access)

  firstT AuthenticationDbError . Traction.runDb pool $
    SessionDb.newSession session (userIdOf u)
  pure session

data AccessToken =
  AccessToken {
      getAccessToken :: ByteString
    , _getAccessTokenScopes :: [Text]
    } deriving (Eq, Ord, Show)

instance Aeson.FromJSON AccessToken where
  parseJSON =
    Aeson.withObject "AccessToken" $ \o ->
      AccessToken
        <$> (Text.encodeUtf8 <$> o .: "access_token")
        <*> (Text.splitOn "," <$> o .: "scope")
