{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Service.Snooze (
    snooze
  , Duration
  , microseconds
  , milliseconds
  , seconds
  , minutes
  , hours
  , toMicroseconds
  , toMilliseconds
  , toHours
  , toMinutes
  , toSeconds
  ) where

import           Boris.Prelude

import           Control.Concurrent
import           System.IO

snooze :: Duration -> IO ()
snooze =
  threadDelay . toMicroseconds


-- |
-- A Duration is an abstract type, representing a short delay (in the
-- region of micro-seconds to a few minutes).
--
-- This useful for implementing concurrency and process primitives
-- that need to wait for short periods to ensure fairness (for example).
--
newtype Duration =
  Duration {
      duration :: Int
    } deriving (Eq, Show)

microseconds :: Int -> Duration
microseconds =
  Duration

milliseconds :: Int -> Duration
milliseconds =
  microseconds . (*) 1000

seconds :: Int -> Duration
seconds =
  milliseconds . (*) 1000

minutes :: Int -> Duration
minutes =
  seconds . (*) 60

hours :: Int -> Duration
hours = minutes . (*) 60

toMicroseconds :: Duration -> Int
toMicroseconds =
  duration

toMilliseconds :: Duration -> Int
toMilliseconds =
  flip div 1000 . toMicroseconds

toSeconds :: Duration -> Int
toSeconds =
  flip div 1000 . toMilliseconds

toMinutes :: Duration -> Int
toMinutes =
  flip div 60 . toSeconds

toHours :: Duration -> Int
toHours =
  flip div 60 . toMinutes
