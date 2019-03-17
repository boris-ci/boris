{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Core.Data.Log (
    Log (..)
  ) where

import           Boris.Prelude

import           Data.Text (Text)
import           Data.Time (UTCTime)

data Log =
    LogEvent !UTCTime !Text
  | LogEOF
    deriving (Eq, Ord, Show)
