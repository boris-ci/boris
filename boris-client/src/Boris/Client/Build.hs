{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Build (
    trigger
  , cancel
  , fetch
  , list
  , ignore
  , rebuild
  , GetBuild (..)
  ) where

import           Boris.Core.Data
import           Boris.Store.Build (BuildData (..), LogData (..), BuildCancelled (..))
import           Boris.Client.Http (BorisHttpClientError (..))
import qualified Boris.Client.Http as H

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))

import           Jebediah.Data (LogGroup (..), LogStream (..))

import           P

import           Snooze.Balance.Control (BalanceConfig)

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)

trigger :: BalanceConfig -> Project -> Build -> Maybe Ref -> EitherT BorisHttpClientError IO BuildData
trigger c p b r =
  fmap getBuild $
    H.post c ["project", renderProject p , "build", renderBuild b] (PostBuildRequest r)

fetch :: BalanceConfig -> BuildId -> EitherT BorisHttpClientError IO (Maybe BuildData)
fetch c i =
  (fmap . fmap) getBuild $
    H.get c ["build", renderBuildId i]

cancel :: BalanceConfig -> BuildId -> EitherT BorisHttpClientError IO ()
cancel c i =
  H.delete c ["build", renderBuildId i]

list :: BalanceConfig -> Project -> Build -> EitherT BorisHttpClientError IO [(Ref, [BuildId])]
list c p b =
  fmap (maybe [] (fmap (\r -> (getBuildsDetailRef r, getBuildsDetailIds r))) . fmap getBuildsDetail) $
    H.get c ["project", renderProject p , "build", renderBuild b]

ignore :: BalanceConfig -> Project -> Build -> Bool -> EitherT BorisHttpClientError IO ()
ignore c p b i =
  H.put c ["project", renderProject p , "build", renderBuild b, "ignore"] (PutBuildIgnore i)

rebuild :: BalanceConfig -> BuildId -> EitherT BorisHttpClientError IO (Maybe BuildData)
rebuild c i = do
  m <- fetch c i
  case m of
    Nothing ->
      pure Nothing
    Just d ->
      (fmap . fmap) getBuild $
        H.post c [
            "project", renderProject $ buildDataProject d
          , "build", renderBuild $ buildDataBuild d
          ] (PostBuildRequest $ buildDataRef d)


newtype PostBuildRequest =
  PostBuildRequest (Maybe Ref)

instance ToJSON PostBuildRequest where
  toJSON (PostBuildRequest r) =
    object [
        "ref" .= fmap renderRef r
      ]

newtype GetBuild =
  GetBuild {
      getBuild :: BuildData
    } deriving (Eq, Show)

instance FromJSON GetBuild where
  parseJSON =
    withObject "GetBuild" $ \o ->
      fmap GetBuild $
        BuildData
          <$> (fmap BuildId $ o .: "build_id")
          <*> (fmap Project $ o .: "project")
          <*> (fmap Build $ o .: "build")
          <*> ((fmap . fmap) Ref $ o .:? "ref")
          <*> ((fmap . fmap) Commit $ o .:? "commit")
          <*> (o .:? "queued")
          <*> (o .:? "started")
          <*> (o .:? "completed")
          <*> (o .:? "heartbeat")
          <*> ((fmap . fmap) (bool BuildKo BuildOk) $ o .:? "result")
          <*> (do ll <- o .:? "log"
                  forM ll $ \l ->
                    flip (withObject "LogData") l $ \ld ->
                      LogData <$> (fmap LogGroup $ ld .: "group") <*> (fmap LogStream $ ld .: "stream"))
          <*> ((fmap . fmap) (bool BuildNotCancelled BuildCancelled) $ o .:? "cancelled")

newtype GetBuilds =
  GetBuilds {
      getBuildsDetail :: [GetBuildsDetail]
    } deriving (Eq, Show)

data GetBuildsDetail =
  GetBuildsDetail {
      getBuildsDetailRef :: Ref
    , getBuildsDetailIds :: [BuildId]
    } deriving (Eq, Show)

instance FromJSON GetBuilds where
  parseJSON =
    withObject "GetBuilds" $ \o ->
      fmap GetBuilds $
        o .: "details"

instance FromJSON GetBuildsDetail where
  parseJSON =
    withObject "GetBuildsDetail" $ \o ->
      GetBuildsDetail
        <$> fmap Ref (o .: "ref")
        <*> (fmap . fmap) BuildId (o .: "build_ids")

newtype PutBuildIgnore =
  PutBuildIgnore
    Bool
      deriving (Eq, Show)

instance ToJSON PutBuildIgnore where
  toJSON (PutBuildIgnore i) =
    object [
        "ignore" .= i
      ]
