{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Http.Data (
    ConfigLocation (..)
  , ClientLocale (..)
  ) where

import           Data.Time (TimeZone)

import           Mismi.S3 (Address)

import           P


newtype ConfigLocation =
  ConfigLocation {
      configLocationAddress :: Address
    } deriving (Eq, Show)

data ClientLocale =
  ClientLocale {
      clientLocaleTimeZone :: TimeZone
    } deriving (Eq, Show)
