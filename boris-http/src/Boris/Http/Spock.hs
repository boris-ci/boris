{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Spock (
    Accept (..)
  , withAccept

  , ContentType (..)
  , withContentType

  , liftError
  , liftStoreError

  , Authenticated (..)
  , authenticated
  , withAuthentication
  ) where

import           Boris.Http.Data
import           Boris.Http.Boot (AuthenticationMode (..))
import qualified Boris.Http.View as View
import qualified Boris.Http.Api.Session as Session
import           Boris.Http.Store.Data
import qualified Boris.Http.Store.Api as Store
import qualified Boris.Http.Store.Error as Store

import           Control.Monad.IO.Class (MonadIO (..))

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Map as Map

import qualified Network.HTTP.Types as HTTP

import           P

import           System.IO (IO)
import qualified System.IO as IO

import qualified Web.Spock.Core as Spock

import           X.Control.Monad.Trans.Either (EitherT, runEitherT)

liftError :: (e -> Text) -> EitherT e IO a -> Spock.ActionT IO a
liftError render value =
  liftIO (runEitherT value) >>= \case
    Left err -> do
      eid <- liftIO newErrorId
      liftIO $ Text.hPutStrLn IO.stderr (mconcat $ [errorId eid, " ", "server error: ", render err])
      Spock.setStatus HTTP.status500
      View.render (View.serverError eid)
    Right v ->
      pure v

liftStoreError :: EitherT Store.StoreError IO a -> Spock.ActionT IO a
liftStoreError =
  liftError Store.renderStoreError

data Accept =
    AcceptHTML
  | AcceptJSON
    deriving (Eq, Ord, Show)

withAccept :: (Accept -> Spock.ActionT IO a) -> Spock.ActionT IO a
withAccept handler = do
  h <- Spock.header "Accept"
  case h of
    Nothing ->
      handler AcceptJSON
    Just accept ->
      let
         (mime, _) = Text.breakOn ";" accept
         mimeTypes = (Text.toLower . Text.strip) <$> Text.splitOn "," mime
         types = Map.fromList [
             ("text/html", AcceptHTML)
           , ("text/json", AcceptJSON)
           , ("application/json", AcceptJSON)
           , ("application/vnd.ambiata.boris.v1+json", AcceptJSON)
           ]

         finder = \case
           [] ->
             AcceptJSON
           (x:xs) ->
             fromMaybe (finder xs) $
               Map.lookup x types
      in
        handler $ finder mimeTypes


data ContentType =
    ContentTypeForm
  | ContentTypeJSON
    deriving (Eq, Ord, Show)

withContentType :: (ContentType -> Spock.ActionT IO a) -> Spock.ActionT IO a
withContentType handler = do
  h <- Spock.header "Content-Type"
  case h of
    Nothing ->
      handler ContentTypeJSON
    Just accept ->
      let
         (mime, _) = Text.breakOn ";" accept
         mimeTypes = (Text.toLower . Text.strip) <$> Text.splitOn "," mime
         types = Map.fromList [
             ("application/x-www-form-urlencoded", ContentTypeForm)
           , ("text/json", ContentTypeJSON)
           , ("application/json", ContentTypeJSON)
           , ("application/vnd.ambiata.boris.v1+json", ContentTypeJSON)
           ]
         finder = \case
           [] ->
             ContentTypeJSON
           (x:xs) ->
             fromMaybe (finder xs) $
               Map.lookup x types
      in
        handler $ finder mimeTypes

data Authenticated =
    Authenticated SessionId UserId
  | AuthenticatedNone
  | NotAuthenticated
  | WasAuthenticated SessionId
    deriving (Eq, Show)

data AuthenticatedBy =
    AuthenticatedByOAuth SessionId UserId
  | AuthenticatedByDesign
    deriving (Eq, Show)

authenticated :: AuthenticationMode -> Store -> (AuthenticatedBy -> Spock.ActionT IO ()) -> Spock.ActionT IO ()
authenticated mode store handler =
  withAuthentication mode store $ \x -> case x of
    Authenticated s u ->
      handler (AuthenticatedByOAuth s u)
    AuthenticatedNone ->
      handler AuthenticatedByDesign
    NotAuthenticated -> do
      Spock.setStatus HTTP.status403
      Spock.html "not authorized"
    WasAuthenticated _ -> do
      -- FIX Add expiry message.
      Spock.setStatus HTTP.status403
      Spock.html "not authorized"

withAuthentication :: AuthenticationMode -> Store -> (Authenticated -> Spock.ActionT IO ()) -> Spock.ActionT IO ()
withAuthentication mode store handler =
  case mode of
    GithubAuthentication manager client secret -> do
      v <- fmap SessionId <$> Spock.cookie "boris"
      case v of
        Nothing ->
          handler NotAuthenticated
        Just session -> do
          account' <- liftError Session.renderAuthenticationError $
            Session.check store manager client secret session
          case account' of
            Nothing ->
              handler $ WasAuthenticated session
            Just account ->
              handler $ Authenticated session account
    NoAuthentication ->
      handler AuthenticatedNone
