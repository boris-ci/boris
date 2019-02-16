{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Core.Serial.Command (
    BorisConfigError (..)
  , renderBorisConfigError
  ) where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Configuration
import           Boris.Prelude

import           Text.Parsec.Error (ParseError)


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
  ZZZ
  {-
    ConfigTomlParseError ParseError
  | ConfigMissingVersionError
  | ConfigUnknownVersionError Int64
  | ConfigInvalidCommand Build
  | ConfigBuildsTypeError
  | ConfigInvalidName Text -}
    deriving (Eq, Show)

parseConfig :: Text -> Either BorisConfigError [Specification]
parseConfig t =
--  first ConfigTomlParseError (parseTomlDoc "boris.toml" t) >>= parseTomlConfig
  error "todo"

renderBorisConfigError :: BorisConfigError -> Text
renderBorisConfigError err =
  error "todo"
