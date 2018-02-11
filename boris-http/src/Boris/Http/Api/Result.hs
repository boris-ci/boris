{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Api.Result (
    scoreboard
  , status
  ) where


import           Boris.Core.Data
import           Boris.Http.Store.Data
import qualified Boris.Http.Store.Api as Store
import qualified Boris.Http.Store.Error as Store

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)


import           Control.Monad.IO.Class (liftIO)
import qualified System.IO as IO


scoreboard :: Store -> EitherT Store.StoreError IO [Result]
scoreboard store = do
  x <- Store.results store
  liftIO . IO.print $ x
  pure x

status :: Store -> EitherT Store.StoreError IO [Result]
status store =
  filter ((==) BuildKo . resultBuildResult)
    <$> scoreboard store
