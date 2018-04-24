{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Core.Data.Time (
    renderTime
  ) where

import qualified Data.Text as Text
import           Data.Time (UTCTime, formatTime, defaultTimeLocale)

import           P


renderTime :: UTCTime -> Text
renderTime =
  Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
