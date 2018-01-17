{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Http.Data (
    ConfigurationMode (..)
  , ClientLocale (..)
  ) where

import           Data.Time.Zones (TZ)

import           Mismi.S3 (Address)

import           P


data ConfigurationMode =
    GlobalS3WhitelistMode Address
    deriving (Eq, Show)

newtype ClientLocale =
  ClientLocale {
      clientLocaleTZ :: TZ
    } deriving (Eq, Show)
