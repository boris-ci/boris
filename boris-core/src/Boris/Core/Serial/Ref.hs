{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Core.Serial.Ref (
    BorisPatternConfigError (..)
  , parsePatternConfig
  , parsePatternConfigExample
  , renderBorisPatternConfigError
  ) where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Configuration
import qualified Boris.Core.Serial.Toml as Toml

import           Boris.Prelude

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as Text

import qualified Toml
import           Toml ((.=))

import           System.IO (IO)



{--
# example "boris-git.toml" config file

[boris]
  version = 1

[build.dist]
  git = "refs/heads/master"

[build.branches]
  git = "refs/heads/topic/*"

[build.hack]
  git = "refs/heads/topic/hack"

[build.success]
  git = "refs/heads/topic/hack"

[build.validate]
  git = "refs/heads/topic/hack"

--}


data BorisPatternConfigError =
  BorisPatternConfigTomlError Toml.DecodeException
{--    PatternConfigTomlParseError ParseError
  | PatternConfigMissingVersionError
  | PatternConfigUnknownVersionError Int64
  | PatternConfigNoReference BuildNamePattern
  | PatternConfigInvalidCommand Build
  | PatternConfigBuildsTypeError
  | PatternConfigBuildNamePatternParseError Text --}
    deriving (Eq, Show)

newtype BorisGitV1 =
  BorisGitV1 {
      refPatterns :: Map Text Text
    } deriving (Eq, Ord, Show)

borisGitV1 :: Toml.TomlCodec BorisGitV1
borisGitV1 =
  BorisGitV1 <$>
    Toml.mapping patternV1 "build" .= refPatterns

patternV1 :: Toml.TomlCodec Text
patternV1 =
  Toml.text "git"

parsePatternConfig :: Text -> Either BorisPatternConfigError BorisGitV1
parsePatternConfig t =
  first BorisPatternConfigTomlError $
    Toml.decode borisGitV1 t

parsePatternConfigExample :: IO (Either BorisPatternConfigError BorisGitV1)
parsePatternConfigExample =
  parsePatternConfig <$> Text.readFile "test/data/config/ref/v1/multiple.toml"

renderBorisPatternConfigError :: BorisPatternConfigError -> Text
renderBorisPatternConfigError err =
  error "todo"
{--  case err of
    PatternConfigTomlParseError p ->
      mconcat ["Boris configuration could not be parsed, toml parse error: ", T.pack . show $ p]
    PatternConfigMissingVersionError ->
      "Boris configuration does not contain a version field."
    PatternConfigUnknownVersionError n ->
      mconcat ["Boris configuration contains an unkown version: ", T.pack . show $ n]
    PatternConfigNoReference b ->
      mconcat ["Boris configuration does not contain a mandatory 'refs' for build: ", renderBuildNamePattern b]
    PatternConfigInvalidCommand b ->
      mconcat ["Boris configuration contains an invalid 'command' for build: ", renderBuild b]
    PatternConfigBuildsTypeError ->
      mconcat ["Boris configuration should contain a top level table 'build'."]
    PatternConfigBuildNamePatternParseError e ->
      mconcat ["Boris configuration build name pattern could not be parsed, parse error: .", e]
--}
