{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Representation.Queue (
    GetQueue (..)
  ) where

import           Boris.Queue (QueueSize (..))

import           Data.Aeson (ToJSON (..), object, (.=))


newtype GetQueue =
  GetQueue QueueSize

instance ToJSON GetQueue where
  toJSON (GetQueue q) =
    object ["size" .= getQueueSize q]
