{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Discover (
    complete
  ) where

import           Boris.Client.Http (BorisHttpClientError (..))
import qualified Boris.Client.Http as H
import           Boris.Core.Data.Build
import           Boris.Core.Data.Instance
import           Boris.Core.Data.Project

import           Boris.Representation.ApiV1

import           P

import           Snooze.Balance.Control (BalanceConfig)

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)


complete :: BalanceConfig -> BuildId -> Project -> [DiscoverInstance] -> EitherT BorisHttpClientError IO ()
complete config i p ds = do
  H.post config ["discover", renderBuildId i] $
    (PostDiscover p ds)
