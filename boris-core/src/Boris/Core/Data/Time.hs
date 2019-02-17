{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Core.Data.Time (
    renderTime
  ) where

import           Boris.Prelude

import qualified Data.Text as Text
import           Data.Time (UTCTime, formatTime, defaultTimeLocale)


renderTime :: UTCTime -> Text
renderTime =
  Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
