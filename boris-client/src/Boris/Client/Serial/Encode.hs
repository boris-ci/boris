{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Client.Serial.Encode (
    auto
  ) where

import qualified Data.Aeson as Aeson

import           Boris.Representation.ApiV1


auto :: Aeson.ToJSON a => a -> Aeson.Value
auto =
  Aeson.toJSON
