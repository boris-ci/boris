{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Http.Data (
    ConfigLocation (..)
  ) where

import           Mismi.S3 (Address)

import           P


newtype ConfigLocation =
  ConfigLocation {
      configLocation :: Address
    } deriving (Eq, Show)
