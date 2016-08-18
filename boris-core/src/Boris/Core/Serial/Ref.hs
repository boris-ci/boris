{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Core.Serial.Ref (
    BorisPatternConfigError (..)
  , parsePatternConfig
  , renderBorisPatternConfigError
  ) where

import           Boris.Core.Data

import           Control.Lens ((^?))

import qualified Data.Text as T
import qualified Data.HashMap.Strict as M

import           Text.Parsec.Error (ParseError)
import           Text.Toml (parseTomlDoc)
import           Text.Toml.Types (Table)

import           P

import           X.Text.Toml (_NTable, _NTValue, _VString, _VInteger, key)

{--
# example config file

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
    PatternConfigTomlParseError ParseError
  | PatternConfigMissingVersionError
  | PatternConfigUnknownVersionError Int64
  | PatternConfigNoReference BuildNamePattern
  | PatternConfigInvalidCommand Build
  | PatternConfigBuildsTypeError
  | PatternConfigBuildNamePatternParseError Text
    deriving (Eq, Show)

parsePatternConfig :: Text -> Either BorisPatternConfigError [BuildPattern]
parsePatternConfig t =
  first PatternConfigTomlParseError (parseTomlDoc "boris-git.toml" t) >>= parseTomlConfig

parseTomlConfig :: Table -> Either BorisPatternConfigError [BuildPattern]
parseTomlConfig t =
  case t ^? key "boris" . _NTable . key "version" . _NTValue . _VInteger of
    Nothing ->
      Left PatternConfigMissingVersionError
    Just 1 ->
      parseTomlConfigV1 t
    Just n ->
      Left $ PatternConfigUnknownVersionError n

parseTomlConfigV1 :: Table -> Either BorisPatternConfigError [BuildPattern]
parseTomlConfigV1 t =
  parseBuilds t >>= \builds ->
    forM (M.keys builds) $ \k -> do
      build <- first PatternConfigBuildNamePatternParseError $ parseBuildNamePattern k
      BuildPattern build
        <$> parseGit builds build

parseBuilds :: Table -> Either BorisPatternConfigError Table
parseBuilds doc =
  case doc ^? key "build" of
    Nothing ->
      pure M.empty
    Just tt ->
      maybeToRight PatternConfigBuildsTypeError $
        tt ^? _NTable

parseGit :: Table -> BuildNamePattern -> Either BorisPatternConfigError Pattern
parseGit builds build =
  fmap Pattern . maybeToRight (PatternConfigNoReference build) $
    builds ^? key (renderBuildNamePattern build) . _NTable . key "git" . _NTValue . _VString

renderBorisPatternConfigError :: BorisPatternConfigError -> Text
renderBorisPatternConfigError err =
  case err of
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
