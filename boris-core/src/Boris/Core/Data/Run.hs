{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Core.Data.Run (
    RunType
  , runTypeToInt
  , runTypeFromInt

  , RunId
  ) where

import           Boris.Prelude


data RunType =
    IsBuild
  | IsDiscover
    deriving (Eq, Ord, Show)


runTypeToInt :: RunType -> Int64
runTypeToInt r =
  case r of
    IsBuild ->
      0
    IsDiscover ->
      1

runTypeFromInt :: Int64 -> Maybe RunType
runTypeFromInt r =
  case r of
    0 ->
      Just IsBuild
    1 ->
      Just IsDiscover
    _ ->
      Nothing


newtype RunId =
  RunId {
      getRunId :: Int64
    } deriving (Eq, Ord, Show)
