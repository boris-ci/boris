{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Log (
    source
  , source'
  ) where

import           Boris.Client.Http (BorisHttpClientError (..))
import qualified Boris.Client.Http as H
import           Boris.Core.Data

import           Data.Conduit (Source, (=$=))
import qualified Data.Conduit.List as CL

import qualified Data.Text as T

import qualified Jebediah.Conduit as J
import           Jebediah.Data (Following (..), Log (..), LogGroup (..), LogStream (..), Query (..))

import           Mismi.Amazonka (Env)

import           P

import           System.IO (IO)

import           Snooze.Balance.Control (BalanceConfig)

import           Twine.Snooze (seconds)

import           X.Control.Monad.Trans.Either (EitherT)

log :: BalanceConfig -> BuildId -> EitherT BorisHttpClientError IO (Maybe LogData)
log c i =
  fmap _ $
    H.get c ["log", renderBuildId i]

source :: Env -> Environment -> BuildId -> Source IO Text
source env e i =
  let
    gname = LogGroup . T.intercalate "." $ ["boris", renderEnvironment e]
    sname = LogStream $ renderBuildId i
  in
    source' env gname sname

source' :: Env -> LogGroup -> LogStream -> Source IO Text
source' env gname sname =
  J.source env gname sname Everything (Follow . seconds $ 1)
    =$= J.unclean
    =$= CL.map logChunk
