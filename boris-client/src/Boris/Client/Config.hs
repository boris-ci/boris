{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Config (
  -- * Boris client runtime data.
    Boris (..)

  -- * Boris client input configuration.
  , BorisEndpoint (..)
  , BorisCredentialsType (..)
  , BorisCredentials (..)

  -- * OAuth2 scopes
  , BorisScope (..)

  -- * High-level configuration operations.
  , configure
  , configureWith
  , configureT
  , configureWithT

  -- * Low-level configuration operations.
  , configureEndpoint
--  , configureOAuth2
--  , configureCredentials
--  , configureCredentialsByteString
--  , configureCredentialsFile

  -- * Configuration errors.
  , BorisConfigureError (..)
  , renderBorisConfigureError
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)

--import           Crypto.JWT (JWK)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson ((.:))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
-- import qualified Network.OAuth2.JWT.Client as OAuth2

import qualified System.Directory as Directory
import qualified System.Environment as Environment
import           System.FilePath (FilePath, (</>))


data Boris =
    Boris BorisEndpoint HTTP.Manager {-- OAuth2.Store --}


newtype BorisEndpoint =
  BorisEndpoint {
      getBorisEndpoint :: Text
    } deriving (Eq, Ord, Show)


data BorisCredentialsType =
    EnvironmentCredentials
  | BorisHomeCredentials FilePath
  | HomeCredentials FilePath
  | SuppliedCredentials
    deriving (Eq, Ord, Show)


data BorisCredentials =
    BorisCredentials BorisCredentialsType -- IdentityId JWK
    deriving (Eq, Show)


data BorisScope =
    ProfileScope
  | BuildScopea
    deriving (Eq, Ord, Show, Enum, Bounded)

{--
toOAuth2 :: BorisScope -> OAuth2.Scope
toOAuth2 s =
  case s of
    ProfileScope ->
      OAuth2.Scope "profile"
    BuildScope ->
      OAuth2.Scope "build"
--}

configure :: IO (Either BorisConfigureError Boris)
configure =
  runExceptT configureT


configureT :: ExceptT BorisConfigureError IO Boris
configureT = do
  liftIO (HTTP.newManager HTTP.tlsManagerSettings) >>=
    configureWithT [minBound .. maxBound]


configureWith :: [BorisScope] -> HTTP.Manager -> IO (Either BorisConfigureError Boris)
configureWith scopes manager =
  runExceptT $ configureWithT scopes manager


configureWithT :: [BorisScope] -> HTTP.Manager -> ExceptT BorisConfigureError IO Boris
configureWithT scopes manager = do
  endpoint <- liftIO $ configureEndpoint
--  BorisCredentials _ issuer jwk <- configureCredentials
--  oauth2 <- liftIO $ configureOAuth2 manager endpoint (toOAuth2 <$> scopes) jwk issuer
  pure $ Boris endpoint manager -- oauth2


configureEndpoint :: IO BorisEndpoint
configureEndpoint =
  maybe (BorisEndpoint "https://boris.mth.io") (BorisEndpoint . Text.pack) <$>
    Environment.lookupEnv "BORIS_ENDPOINT"

{--
configureOAuth2 :: HTTP.Manager -> BorisEndpoint -> [OAuth2.Scope] -> JWK -> IdentityId -> IO OAuth2.Store
configureOAuth2 manager endpoint scopes jwk issuer =
  let
    token =
      OAuth2.TokenEndpoint $
        mconcat [getBorisEndpoint endpoint, "/oauth/token"]

    claims =
      OAuth2.Claims
        (OAuth2.Issuer $ identityId issuer)
        Nothing
        (OAuth2.Audience "https://boris.mth.io")
        scopes
        (OAuth2.ExpiresIn 3600)
        []
   in
     OAuth2.newStore manager token claims jwk
--}

{--
configureCredentials :: ExceptT BorisConfigureError IO BorisCredentials
configureCredentials = do
  j <- liftIO $ Environment.lookupEnv "BORIS_JWK"
  case j of
    Nothing -> do
      s <- liftIO $ Environment.lookupEnv "BORIS_HOME"
      case s of
        Nothing -> do
          h <- liftIO Directory.getHomeDirectory
          let path = h </> ".boris" </> "credentials.json"
          configureCredentialsFile (BorisHomeCredentials path) path
        Just ss -> do
          let path = ss </> "credentials.json"
          configureCredentialsFile (BorisHomeCredentials path) path
    Just s ->
      configureCredentialsByteString EnvironmentCredentials . Text.encodeUtf8 . Text.pack $ s
--}

{--
configureCredentialsByteString :: BorisCredentialsType -> ByteString -> ExceptT BorisConfigureError IO BorisCredentials
configureCredentialsByteString t keydata = do
  json <- case Aeson.decodeStrict keydata of
    Nothing ->
      left $ BorisConfigureJsonDecodeError t
    Just v ->
      pure v

  jwk <- case Aeson.decodeStrict keydata of
    Nothing ->
      left $ BorisConfigureJwkDecodeError t
    Just v ->
      pure v

  issuer <- case Aeson.parse (Aeson.withObject "JWK" $ \o -> o .: "boris.mth.io/identity-id") json of
    Aeson.Error _msg ->
      left $ BorisConfigureIdentityIdDecodeError t
    Aeson.Success v ->
      pure $ IdentityId . Text.pack . show $ (v :: Int64)

  pure $ BorisCredentials t issuer jwk

--}

{--
configureCredentialsFile :: BorisCredentialsType -> FilePath -> ExceptT BorisConfigureError IO BorisCredentials
configureCredentialsFile t path = do
  exists <- liftIO . Directory.doesFileExist $ path
  case exists of
    False ->
      left $ BorisConfigureCredentialsNotFound t
    True -> do
      bytes <- liftIO . ByteString.readFile $ path
      configureCredentialsByteString t bytes
--}

data BorisConfigureError =
    BorisConfigureCredentialsNotFound BorisCredentialsType
  | BorisConfigureJsonDecodeError BorisCredentialsType
  | BorisConfigureJwkDecodeError BorisCredentialsType
  | BorisConfigureIdentityIdDecodeError BorisCredentialsType
    deriving (Eq, Ord, Show)


renderBorisCredentialsType :: BorisCredentialsType -> Text
renderBorisCredentialsType t =
  case t of
    EnvironmentCredentials ->
      "Credentials were found in environment using $BORIS_JWK."
    BorisHomeCredentials path ->
      mconcat ["Credentials were found using $BORIS_HOME: ", Text.pack path]
    HomeCredentials path ->
      mconcat ["Credentials were found using $HOME: ", Text.pack path]
    SuppliedCredentials ->
      "Credentials were supplied programatically."


renderBorisConfigureError :: BorisConfigureError -> Text
renderBorisConfigureError e =
  case e of
    BorisConfigureCredentialsNotFound t ->
      mconcat ["Boris credentials not found. ", case t of
        SuppliedCredentials ->
          "It looks you are using the library programatically, consider using 'configure'."
        BorisHomeCredentials path ->
          mconcat ["$BORIS_HOME is set, using $BORIS_HOME/credentials.json, but credentials file was not found: ", Text.pack path]
        HomeCredentials path ->
          mconcat ["$BORIS_HOME is not set, defaulting to $HOME/.boris/credentials.json, but credentials file was not found: ", Text.pack path]
        EnvironmentCredentials ->
          "Attempted to use $BORIS_JWK, but it was not set."]
    BorisConfigureJsonDecodeError t ->
      mconcat ["Boris credentials are not valid json and could not be decoded. ", renderBorisCredentialsType t]
    BorisConfigureJwkDecodeError t ->
      mconcat ["Boris credentials do not contain a valid JWT and could not be decoded. ", renderBorisCredentialsType t]
    BorisConfigureIdentityIdDecodeError t ->
      mconcat ["Boris credentials do not contain a valid identity and could not be decoded. ", renderBorisCredentialsType t]


left :: Applicative m => x -> ExceptT x m a
left =
  ExceptT . pure . Left
