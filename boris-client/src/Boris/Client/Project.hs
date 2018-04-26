{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Project (
    list
  , fetch
  , discover
  ) where

import           Boris.Client.Http (BorisHttpClientError (..))
import qualified Boris.Client.Http as H
import           Boris.Core.Data.Build
import           Boris.Core.Data.Project
import           Boris.Representation.ApiV1

import           P

import           Snooze.Balance.Control (BalanceConfig)

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)

list :: BalanceConfig -> EitherT BorisHttpClientError IO [Project]
list c =
  fmap (maybe [] getProjects) $
    H.get c ["project"]

fetch :: BalanceConfig -> Project -> EitherT BorisHttpClientError IO [Build]
fetch c p =
  fmap (maybe [] getProjectBuilds) $
    H.get c ["project", renderProject p]

discover :: BalanceConfig -> Project -> EitherT BorisHttpClientError IO ()
discover c p =
  H.post_ c ["project", renderProject p]
