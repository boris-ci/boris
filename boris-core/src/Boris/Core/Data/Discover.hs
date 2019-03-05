{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Core.Data.Discover (
    DiscoverId (..)
  , renderDiscoverId
  , Discover (..)
  ) where

import           Boris.Prelude

import qualified Data.Text as Text


newtype DiscoverId =
  DiscoverId {
      getDiscoverId :: Int64
    } deriving (Eq, Ord, Show)

renderDiscoverId :: DiscoverId -> Text
renderDiscoverId =
  Text.pack . show . getDiscoverId

data Discover =
  Discover {
    } deriving (Eq, Ord, Show)
