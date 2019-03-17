{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Boris.Service.Log (
    LogError
  , renderLogError
  , newLogger
  , withLogger
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Catch (Exception, catch, throwM)

import           Boris.Core.Data.Run
import           Boris.Core.Data.Log
import           Boris.Git.X (Out)
import           Boris.Service.Boot
import           Boris.Prelude
import           Boris.Client.Config (Boris)
import           Boris.Client.Error
import qualified Boris.Client.Network as Network

import           Data.ByteString (ByteString)
import qualified Data.Conduit.Binary as ConduitBinary
import qualified Data.Conduit as Conduit
import           Data.Typeable (Typeable)
import           Data.Time (UTCTime)
import qualified Data.Time as Time


import           System.IO (IO, stdout)

import qualified Data.List as List
import qualified Data.Text as Text

import           Boris.Prelude

import           System.IO (IO)

import           Boris.Git.Pin (Pin, newPin, checkPin, pullPin)
import           Boris.Service.Snooze (snooze, milliseconds, seconds)

data LogError =
    LogHttpError BorisError
    deriving (Typeable, Eq, Show)

instance Exception LogError

renderLogError :: LogError -> Text
renderLogError err =
  case err of
    LogHttpError e ->
      renderBorisError e

withLogger :: LogService -> RunId -> (Out -> IO a) -> EitherT LogError IO a
withLogger service run f =
  liftIO (f (newLogger service run)) `catch` left

newLogger :: LogService -> RunId -> Out
newLogger service run =
  case service of
    Std ->
      ConduitBinary.sinkHandle stdout
    PushLog http ->
      sinker http run

sinker :: Boris -> RunId -> Out
sinker boris run = do
  last <- liftIO Time.getCurrentTime
  sinker' boris run last 0 []

sinker' :: Boris -> RunId -> UTCTime -> Int64 -> [Log] -> Out
sinker' boris run last !n buffer = do
  line <- Conduit.await
  case line of
    Nothing ->
      sinker boris run last n buffer
    Just LogEOF -> do
      r <- Network.runRequest http $
        Log.push (List.reverse $ LogEOF : buffer)
      case r of
        Left err ->
          throwM err
        Right v ->
          pure v
    Just e@(LogEvent _ _) -> do
      now <- liftIO Time.getCurrentTime
      let all = a : buffer
      if Time.diffUTCTime now last > 5 || n > 25 then do
        r <- Network.runRequest http $
          Log.push (List.reverse $ e : buffer)
        case r of
          Left err ->
            throwM err
          Right v ->
            pure v
        sinker boris run now 0 []
      else
        sinker boris run last (n + 1) (e : buffer)
