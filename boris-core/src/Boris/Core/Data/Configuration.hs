{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
module Boris.Core.Data.Configuration (
    Command (..)
  , BuildPattern (..)
  , Specification (..)
  , BuildNamePattern
  , renderBuildNamePattern
  , parseBuildNamePattern
  , matchesBuild
  ) where

import           Boris.Core.Data.Build
import           Boris.Prelude

import qualified Data.Text as Text

import qualified "Glob" System.FilePath.Glob as Glob

data Command =
  Command {
      commandName :: Text
    , commandArgs :: [Text]
    } deriving (Eq, Show)

data Specification =
  Specification {
      specificationBuild :: BuildName
    , specificationPre :: [Command]
    , specificationCommand :: [Command]
    , specificationPost :: [Command]
    , specificationSuccess :: [Command]
    , specificationFailure :: [Command]
    } deriving (Eq, Show)

data BuildPattern =
  BuildPattern {
      buildNamePattern :: BuildNamePattern
    , buildPattern :: Pattern
    } deriving (Eq, Show)

newtype BuildNamePattern =
  BuildNamePattern {
      _getBuildNamePattern :: Glob.Pattern
    } deriving (Eq, Show)

renderBuildNamePattern :: BuildNamePattern -> Text
renderBuildNamePattern (BuildNamePattern g) =
  Text.pack . Glob.decompile $ g

parseBuildNamePattern :: Text -> Either Text BuildNamePattern
parseBuildNamePattern =
  let
    options =
      Glob.CompOptions {
          Glob.characterClasses = False
        , Glob.characterRanges = False
        , Glob.numberRanges = False
        , Glob.wildcards = True
        , Glob.recursiveWildcards = False
        , Glob.pathSepInRanges = False
        , Glob.errorRecovery = False
        }
  in
    bimap Text.pack BuildNamePattern . Glob.tryCompileWith options . Text.unpack

matchesBuild :: BuildNamePattern -> BuildName -> Bool
matchesBuild (BuildNamePattern glob) build =
  Glob.match
    glob
    (Text.unpack $ renderBuildName build)
