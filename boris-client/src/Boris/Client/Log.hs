{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Log (
    fetch
  ) where

import           Boris.Client.Http (BorisHttpClientError (..))
import qualified Boris.Client.Http as H
import           Boris.Core.Data.Build
import           Boris.Core.Data.Log
import           Boris.Representation.ApiV1

import           P

import           System.IO (IO)

import           Snooze.Balance.Control (BalanceConfig)

import           X.Control.Monad.Trans.Either (EitherT)

fetch :: BalanceConfig -> BuildId -> EitherT BorisHttpClientError IO LogData
fetch c i =
  fmap (maybe (DBLog []) getLogs) $
    H.get c ["log", renderBuildId i]
