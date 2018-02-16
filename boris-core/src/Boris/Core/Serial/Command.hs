{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Core.Serial.Command (
    BorisConfigError (..)
  , parseConfig
  , renderBorisConfigError
  ) where

import           Boris.Core.Data

import           Control.Lens ((^?))

import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

import           Text.Parsec.Error (ParseError)
import           Text.Toml (parseTomlDoc)
import           Text.Toml.Types (TValue, Table)

import           P

import           X.Text.Toml (key, _NTValue, _NTable, _VArray, _VInteger, _VString)

{--
# example "boris.toml" config file

[boris]
  version = 1

[build.dist]

[build.branches]

[build.hack]
  command = [["./mafia", "test"]]

[build.success]
  success = [["hipchat", "yo"]]

[build.validate]
  pre = [["validate-respect"], ["rebased"]]

--}


data BorisConfigError =
    ConfigTomlParseError ParseError
  | ConfigMissingVersionError
  | ConfigUnknownVersionError Int64
  | ConfigInvalidCommand Build
  | ConfigBuildsTypeError
  | ConfigInvalidName Text
    deriving (Eq, Show)

parseConfig :: Text -> Either BorisConfigError [Specification]
parseConfig t =
  first ConfigTomlParseError (parseTomlDoc "boris.toml" t) >>= parseTomlConfig

parseTomlConfig :: Table -> Either BorisConfigError [Specification]
parseTomlConfig t =
  case t ^? key "boris" . _NTable . key "version" . _NTValue . _VInteger of
    Nothing ->
      Left ConfigMissingVersionError
    Just 1 ->
      parseTomlConfigV1 t
    Just n ->
      Left $ ConfigUnknownVersionError n

parseTomlConfigV1 :: Table -> Either BorisConfigError [Specification]
parseTomlConfigV1 t =
  parseBuilds t >>= \builds ->
    forM (M.keys builds) $ \k -> do
      build <- maybeToRight (ConfigInvalidName k) $ newBuild k
      Specification build
        <$> parseCommands' builds build "pre" []
        <*> parseCommands' builds build "command" []
        <*> parseCommands builds build "post"
        <*> parseCommands' builds build "success" []
        <*> parseCommands' builds build "failure" []

parseBuilds :: Table -> Either BorisConfigError Table
parseBuilds doc =
  case doc ^? key "build" of
    Nothing ->
      pure M.empty
    Just tt ->
      maybeToRight ConfigBuildsTypeError $
        tt ^? _NTable

parseCommands :: Table -> Build -> Text -> Either BorisConfigError [Command]
parseCommands builds build t =
  parseCommands' builds build t []

parseCommands' :: Table -> Build -> Text -> [Command] -> Either BorisConfigError [Command]
parseCommands' builds build t dfault =
  case builds ^? key (renderBuild build) . _NTable . key t . _NTValue . _VArray of
    Nothing ->
      Right dfault
    Just [] ->
      Right []
    Just xs ->
      forM xs $ parseCommand build

parseCommand :: Build -> TValue -> Either BorisConfigError Command
parseCommand build table =
  case table ^? _VArray of
    Nothing ->
      Left $ ConfigInvalidCommand build
    Just [] ->
      Left $ ConfigInvalidCommand build
    Just xs ->
      case forM xs (^? _VString) of
        Nothing ->
          Left $ ConfigInvalidCommand build
        Just [] ->
          Left $ ConfigInvalidCommand build
        Just (cmd:args) ->
          Right $ Command cmd args

renderBorisConfigError :: BorisConfigError -> Text
renderBorisConfigError err =
  case err of
    ConfigTomlParseError p ->
      mconcat ["Boris configuration could not be parsed, toml parse error: ", T.pack . show $ p]
    ConfigMissingVersionError ->
      "Boris configuration does not contain a version field."
    ConfigUnknownVersionError n ->
      mconcat ["Boris configuration contains an unkown version: ", T.pack . show $ n]
    ConfigInvalidCommand b ->
      mconcat ["Boris configuration contains an invalid 'command' for build: ", renderBuild b]
    ConfigBuildsTypeError ->
      mconcat ["Boris configuration should contain a top level table 'build'."]
    ConfigInvalidName n ->
      mconcat ["Boris configuration contains an invalid name: ", n]
