{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Build (
    trigger
  , cancel
  , fetch
  , list
  , ignore
  , rebuild
  , queue
  , heartbeat
  , acknowledge
  , avow
  , complete
  ) where

import           Boris.Client.Http (BorisHttpClientError (..))
import qualified Boris.Client.Http as H
import           Boris.Core.Data.Agent
import           Boris.Core.Data.Build
import           Boris.Core.Data.Project
import           Boris.Representation.ApiV1

import           Data.Aeson (object, (.=))

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

list :: BalanceConfig -> Project -> Build -> EitherT BorisHttpClientError IO (Maybe BuildTree)
list c p b =
  (fmap . fmap) getBuilds $
    H.get c ["project", renderProject p , "build", renderBuild b]

ignore :: BalanceConfig -> Project -> Build -> Bool -> EitherT BorisHttpClientError IO ()
ignore c p b i =
  H.put c ["project", renderProject p , "build", renderBuild b, "ignore"] (PutBuildIgnore i)

heartbeat :: BalanceConfig -> BuildId -> EitherT BorisHttpClientError IO BuildCancelled
heartbeat c i =
  fmap postHeartbeatCancelled $
    H.post c ["build", renderBuildId i, "heartbeat"] ()

acknowledge :: BalanceConfig -> BuildId -> EitherT BorisHttpClientError IO Acknowledge
acknowledge c i =
  fmap postAcknowledge $
    H.post c ["build", renderBuildId i, "acknowledge"] PostAcknowledgeRequest

avow :: BalanceConfig -> BuildId -> Ref -> Commit -> EitherT BorisHttpClientError IO ()
avow config i r c = do
  PostAvowResponse <- H.post config ["build", renderBuildId i, "avow"] $
    PostAvowRequest r c
  pure ()

complete :: BalanceConfig -> BuildId -> BuildResult -> EitherT BorisHttpClientError IO ()
complete config i r = do
  PostCompleteResponse <- H.post config ["build", renderBuildId i, "complete"] $ object [
      "result" .= case r of BuildOk -> True; BuildKo -> False
    ]
  pure ()

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

queue :: BalanceConfig -> EitherT BorisHttpClientError IO (Maybe QueueSize)
queue c =
  (fmap . fmap) getQueue $
    H.get c ["queue"]
