{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Http.Data (
    ConfigLocation (..)
  , ClientLocale (..)
  ) where

import           Data.Time.Zones (TZ)

import           Mismi.S3 (Address)

import           P


newtype ConfigLocation =
  ConfigLocation {
      configLocationAddress :: Address
    } deriving (Eq, Show)

data ClientLocale =
  ClientLocale {
      clientLocaleTZ :: TZ
    } deriving (Eq, Show)
