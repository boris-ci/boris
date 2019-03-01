{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Scoreboard (
    scoreboard
  ) where

import qualified Boris.Client.Response as Response
import           Boris.Client.Request (Request (..))
import qualified Boris.Client.Request as Request
import qualified Boris.Client.Serial.Decode as Decode
import qualified Boris.Client.Serial.Encode as Encode
import           Boris.Core.Data.Build
import           Boris.Core.Data.Project
import           Boris.Prelude


scoreboard :: Request [Result]
scoreboard =
  error "todo"
  {--
  fmap (maybe [] getResults) $
    H.get c ["scoreboard"]

newtype GetScoreboard =
  GetScoreboard {
      getResults :: [Result]
    } deriving (Eq, Show)

instance FromJSON GetScoreboard where
  parseJSON =
    withObject "GetScoreboard" $ \o ->
      fmap GetScoreboard $ o .: "builds" >>= mapM toResult


toResult :: Value -> Parser Result
toResult =
  withObject "Result" $ \o ->
    Result
      <$> (fmap BuildId $ o .: "build_id")
      <*> (fmap Project $ o .: "project")
      <*> (fmap Build $ o .: "build")
      <*> ((fmap . fmap) Ref $ o .:? "ref")
      <*> (o .: "result" >>= \u -> fromMaybeM (fail $ "Unknown BuildResult: " <> T.unpack u) $ parseBuildResult u)
--}
