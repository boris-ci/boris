{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Core.Serial.Ref (
    BorisQueryConfigError (..)
  , parseQueryConfig
  , renderBorisQueryConfigError
  ) where

import           Boris.Core.Data

import           Control.Lens ((^?))

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M

import           Text.Parsec.Error (ParseError)
import           Text.Toml (parseTomlDoc)
import           Text.Toml.Types (Table, TValue)

import           P

import           X.Text.Toml (_NTable, _NTValue, _VArray, _VString, _VInteger, key)

{--
# example config file

[boris]
  version = 1

[build.dist]
  refs = "refs/heads/master"

[build.branches]
  refs = "refs/heads/topic/*"

[build.hack]
  refs = "refs/heads/topic/hack"
  command = [["./mafia", "test"]]

[build.success]
  refs = "refs/heads/topic/hack"
  success = [["hipchat", "yo"]]

[build.validate]
  refs = "refs/heads/topic/hack"
  pre = [["validate-respect"], ["rebased"]]

--}


data BorisQueryConfigError =
    QueryConfigTomlParseError ParseError
  | QueryConfigMissingVersionError
  | QueryConfigUnknownVersionError Int64
  | QueryConfigNoReference Build
  | QueryConfigInvalidCommand Build
  | QueryConfigBuildsTypeError
    deriving (Eq, Show)

parseQueryConfig :: Text -> Either BorisQueryConfigError [BuildQuery]
parseQueryConfig t =
  first QueryConfigTomlParseError (parseTomlDoc "boris-query.toml" t) >>= parseTomlConfig

parseTomlConfig :: Table -> Either BorisQueryConfigError [BuildQuery]
parseTomlConfig t =
  case t ^? key "boris" . _NTable . key "version" . _NTValue . _VInteger of
    Nothing ->
      Left QueryConfigMissingVersionError
    Just 1 ->
      parseTomlConfigV1 t
    Just n ->
      Left $ QueryConfigUnknownVersionError n

parseTomlConfigV1 :: Table -> Either BorisQueryConfigError [BuildQuery]
parseTomlConfigV1 t =
  parseBuilds t >>= \builds ->
    forM (M.keys builds) $ \k ->
      let
        build = Build k
      in
        BuildQuery build
          <$> parseGit builds build

parseBuilds :: Table -> Either BorisQueryConfigError Table
parseBuilds doc =
  case doc ^? key "build" of
    Nothing ->
      pure M.empty
    Just tt ->
      maybeToRight QueryConfigBuildsTypeError $
        tt ^? _NTable

parseGit :: Table -> Build -> Either BorisQueryConfigError Query
parseGit builds build =
  fmap Query . maybeToRight (QueryConfigNoReference build) $
    builds ^? key (renderBuild build) . _NTable . key "git" . _NTValue . _VString

renderBorisQueryConfigError :: BorisQueryConfigError -> Text
renderBorisQueryConfigError err =
  case err of
    QueryConfigTomlParseError p ->
      mconcat ["Boris configuration could not be parsed, toml parse error: ", T.pack . show $ p]
    QueryConfigMissingVersionError ->
      "Boris configuration does not contain a version field."
    QueryConfigUnknownVersionError n ->
      mconcat ["Boris configuration contains an unkown version: ", T.pack . show $ n]
    QueryConfigNoReference b ->
      mconcat ["Boris configuration does not contain a mandatory 'refs' for build: ", renderBuild b]
    QueryConfigInvalidCommand b ->
      mconcat ["Boris configuration contains an invalid 'command' for build: ", renderBuild b]
    QueryConfigBuildsTypeError ->
      mconcat ["Boris configuration should contain a top level table 'build'."]
