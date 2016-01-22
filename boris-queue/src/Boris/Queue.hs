{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Queue (
    QueueError (..)
  , Request (..)
  , BuildQueue (..)
  , put
  , get
  , renderQueueError
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
  Request {
      requestBuildId :: BuildId
    , requestProject :: Project
    , requestRepository :: Repository
    , requestBuild :: Build
    , requestRef :: Maybe Ref
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
fromRequest r =
  object [
      "version" .= ("v1" :: Text)
    , "id" .= (renderBuildId . requestBuildId) r
    , "project" .= (renderProject . requestProject) r
    , "repository" .= (renderRepository . requestRepository) r
    , "build" .= (renderBuild . requestBuild) r
    , "ref" .= (fmap renderRef . requestRef) r
    ]

toRequest :: Value -> Parser Request
toRequest =
  withObject "Request" $ \o ->
    o .: "version" >>= \ver -> case ver of
      "v1" ->
        Request
          <$> (BuildId <$> o .: "id")
          <*> (Project <$> o .: "project")
          <*> (Repository <$> o .: "repository")
          <*> (Build <$> o .: "build")
          <*> (fmap Ref <$> o .:? "ref")
      _ ->
        fail $ "Unknown version: " <> ver

renderQueueError :: QueueError -> Text
renderQueueError err =
  case err of
    QueueMissingBodyError ->
      "Build request did not include a body."
    QueueParseError msg ->
      mconcat ["Build request could not be parsed: ", msg]
