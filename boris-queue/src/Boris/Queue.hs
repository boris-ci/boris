{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Queue (
    QueueError (..)
  , Request (..)
  , RequestBuild (..)
  , RequestDiscover (..)
  , BuildQueue (..)
  , put
  , get
  , renderQueueError
  , fromRequest
  , fromRequestV1
  , fromRequestV2
  , toRequest
  ) where

import           Control.Lens ((^.))
import           Control.Monad.Trans.Class (lift)

import           Boris.Core.Data

import           Data.Aeson (Value (..), object, withObject, (.=), (.:), (.:?))
import           Data.Aeson.Types (Parser)
import           Data.Text (Text)

import           Mismi (AWS)
import qualified Mismi.SQS as Q
import qualified Mismi.SQS.Amazonka as A

import           P


import           X.Control.Monad.Trans.Either (EitherT, left, hoistEither)
import           X.Data.Aeson (asWith, asTextWith)


data QueueError =
    QueueMissingBodyError
  | QueueParseError Text
    deriving (Eq, Show)

data Request =
    RequestBuild' RequestBuild
  | RequestDiscover' RequestDiscover
    deriving (Eq, Show)

data RequestBuild =
  RequestBuild {
      requestBuildId :: BuildId
    , requestBuildProject :: Project
    , requestBuildRepository :: Repository
    , requestBuildName :: Build
    , requestBuildRef :: Maybe Ref
    } deriving (Eq, Show)

data RequestDiscover =
  RequestDiscover {
      requestDiscoverId :: BuildId
    , requestDiscoverProject :: Project
    , requestDiscoverRepository :: Repository
    } deriving (Eq, Show)

newtype BuildQueue =
  BuildQueue {
      renderBuildQueue :: Text
    } deriving (Eq, Show)

put :: BuildQueue -> Request -> AWS ()
put bq request = do
  q <- Q.createQueue (Q.QueueName . renderBuildQueue $ bq) (Just 10)
  void $ Q.writeMessage q (asTextWith fromRequest request) Nothing

get :: BuildQueue -> EitherT QueueError AWS (Maybe Request)
get bq = do
  q <- lift $ Q.createQueue (Q.QueueName . renderBuildQueue $ bq) (Just 10)
  m <- lift . fmap listToMaybe $ Q.readMessages q (Just 1) Nothing
  forM m $ \msg -> do
    body <- fromMaybeM (left QueueMissingBodyError) (msg ^. A.mBody)
    parsed <- hoistEither . first QueueParseError . asWith toRequest $ body
    lift . void $ Q.deleteMessage q msg
    pure parsed

fromRequest :: Request -> Value
fromRequest =
  fromRequestV2

fromRequestV1 :: Request -> Value
fromRequestV1 r =
  case r of
    RequestDiscover' _ ->
      fromRequestV2 r
    RequestBuild' rr ->
      object [
          "version" .= ("v1" :: Text)
        , "id" .= (renderBuildId . requestBuildId) rr
        , "project" .= (renderProject . requestBuildProject) rr
        , "repository" .= (renderRepository . requestBuildRepository) rr
        , "build" .= (renderBuild . requestBuildName) rr
        , "ref" .= (fmap renderRef . requestBuildRef) rr
        ]

fromRequestV2 :: Request -> Value
fromRequestV2 r =
  case r of
    RequestDiscover' rr ->
      object [
          "version" .= ("v2" :: Text)
        , "type" .= ("discover" :: Text)
        , "id" .= (renderBuildId . requestDiscoverId) rr
        , "project" .= (renderProject . requestDiscoverProject) rr
        , "repository" .= (renderRepository . requestDiscoverRepository) rr
        ]
    RequestBuild' rr ->
      object [
          "version" .= ("v2" :: Text)
        , "type" .= ("build" :: Text)
        , "id" .= (renderBuildId . requestBuildId) rr
        , "project" .= (renderProject . requestBuildProject) rr
        , "repository" .= (renderRepository . requestBuildRepository) rr
        , "build" .= (renderBuild . requestBuildName) rr
        , "ref" .= (fmap renderRef . requestBuildRef) rr
        ]

toRequest :: Value -> Parser Request
toRequest =
  withObject "Request" $ \o ->
    o .: "version" >>= \ver -> case ver of
      "v1" ->
        fmap RequestBuild' $ RequestBuild
          <$> (BuildId <$> o .: "id")
          <*> (Project <$> o .: "project")
          <*> (Repository <$> o .: "repository")
          <*> (Build <$> o .: "build")
          <*> (fmap Ref <$> o .:? "ref")
      "v2" ->
        o .: "type" >>= \ty -> case ty of
          "discover" ->
            fmap RequestDiscover'  $ RequestDiscover
              <$> (BuildId <$> o .: "id")
              <*> (Project <$> o .: "project")
              <*> (Repository <$> o .: "repository")
          "build" ->
            fmap RequestBuild' $ RequestBuild
              <$> (BuildId <$> o .: "id")
              <*> (Project <$> o .: "project")
              <*> (Repository <$> o .: "repository")
              <*> (Build <$> o .: "build")
              <*> (fmap Ref <$> o .:? "ref")
          _ ->
            fail $ "Unknown request type: " <> ty
      _ ->
        fail $ "Unknown request version: " <> ver

renderQueueError :: QueueError -> Text
renderQueueError err =
  case err of
    QueueMissingBodyError ->
      "Build request did not include a body."
    QueueParseError msg ->
      mconcat ["Build request could not be parsed: ", msg]
