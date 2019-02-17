{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Core.Data.Log (
    DBLogData (..)
  , LogData (..)
  , renderDBLogData
  , renderDBLogs
  ) where

import           Boris.Core.Data.Time
import           Boris.Prelude

import qualified Data.Text as Text
import           Data.Time (UTCTime)

data LogData =
    DBLog [DBLogData]
    deriving (Eq, Ord, Show)

data DBLogData =
  DBLogData {
      logEntryTimeStamp :: UTCTime
    , logEntry :: Text
    } deriving (Eq, Ord, Show)

renderDBLogData :: DBLogData -> Text
renderDBLogData dbl =
  mconcat [ renderTime $ logEntryTimeStamp dbl, "        ", logEntry dbl ]

renderDBLogs :: [DBLogData] -> Text
renderDBLogs ls =
  Text.intercalate "\n" $ renderDBLogData <$> ls
