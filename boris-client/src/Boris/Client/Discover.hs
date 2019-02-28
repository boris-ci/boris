{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Discover (
    complete
  ) where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Instance
import           Boris.Core.Data.Project

import qualified Boris.Client.Response as Response
import           Boris.Client.Request (Request (..))
import qualified Boris.Client.Request as Request
import qualified Boris.Client.Serial.Decode as Decode
import qualified Boris.Client.Serial.Encode as Encode

import           Boris.Representation.ApiV1

import           P

import           Snooze.Balance.Control (BalanceConfig)

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)


complete :: BalanceConfig -> BuildId -> Project -> [DiscoverInstance] -> EitherT BorisHttpClientError IO ()
complete config i p ds = do
  error "todo"
