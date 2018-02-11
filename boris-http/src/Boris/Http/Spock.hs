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

  ) where

import           Boris.Http.Data
import qualified Boris.Http.View as View
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
