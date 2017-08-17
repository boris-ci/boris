{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Version (
    ApiVersion (..)
  ) where

import           Boris.Http.Airship

import           P


data ApiVersion =
    V1
    deriving (Bounded, Enum, Eq, Show)

instance Versioned ApiVersion where
  versionToMedia V1 = "application/vnd.ambiata.boris.v1+json"
