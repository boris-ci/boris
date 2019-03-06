{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_boris_service
import           DependencyInfo_boris_service

import           Boris.Core.Data.Build
import           Boris.Core.Data.Project
import           Boris.Core.Data.Repository
import           Boris.Core.Data.Workspace
import qualified Boris.Client.Config as Config
import qualified Boris.Service.Boot as Boot
import qualified Boris.Service.Discover as Discover
import           Boris.Prelude

import           Data.Default (def)
import           Data.String (String)
import qualified Data.Text as T
import qualified Data.Text as Text

import qualified Nest

import           Network.Connection (ProxySettings (..))
import           Network.HTTP.Client (ManagerSettings, newManager)
import           Network.HTTP.Client.TLS (mkManagerSettings)

import qualified Options.Applicative as Options
import           Options.Applicative

import           System.Exit (exitSuccess, exitFailure)
import           System.Environment (lookupEnv)
import           System.IO


data Cli =
    BuildCommand BuildId ProjectName Repository BuildName (Maybe Ref)
  | DiscoverCommand BuildId ProjectName Repository
  | Version
    deriving (Eq, Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch (Version <$ versionP <|> parser) >>= run

parser :: Options.Parser Cli
parser =
  Options.hsubparser . mconcat $ [
      command' "build" "Trigger a build" $
        BuildCommand
          <$> buildIdP
          <*> projectP
          <*> repositoryP
          <*> buildP
          <*> (optional refP)

    , command' "discover" "Probe for builds to trigger for a project" $
        DiscoverCommand
          <$> buildIdP
          <*> projectP
          <*> repositoryP
    ]

run :: Cli -> IO ()
run c = case c of
  Version ->
    putStrLn buildInfoVersion >> exitSuccess
  DiscoverCommand buildid project repository -> do
    let
      path =
        WorkspacePath "."

      configure =
        orDie Config.renderBorisConfigureError . newEitherT $
          Config.configure

    Boot.Boot logs discovers _ <-
      Nest.force $ Boot.boot configure

    orDie Discover.renderDiscoverError $
      Discover.discover logs discovers path buildid project repository

    exitSuccess

  BuildCommand _buildid _project _repository _build _ref -> do
    let
      configure =
        orDie Config.renderBorisConfigureError . newEitherT $
          Config.configure

    Boot.Boot _logs _ _build <-
      Nest.force $ Boot.boot configure

    -- TODO implement
    exitSuccess

projectP :: Parser ProjectName
projectP =
  fmap ProjectName . argument textRead . mconcat $ [
      metavar "PROJECT"
    , help "Project name, this relates to the project name configured in boris, e.g. boris."
    ]

repositoryP :: Parser Repository
repositoryP =
  fmap Repository . argument textRead . mconcat $ [
      metavar "REPOSITORY"
    , help "Repository url."
    ]

buildP :: Parser BuildName
buildP =
  fmap BuildName . argument textRead . mconcat $ [
      metavar "BUILD"
    , help "Build name, this relates to the project name configured in repository, e.g. dist, branches."
    ]

refP :: Parser Ref
refP =
  fmap Ref . argument textRead . mconcat $ [
      metavar "REF"
    , help "A specific git ref to build, e.g. master, topic/hax."
    ]

buildIdP :: Parser BuildId
buildIdP =
  fmap BuildId . argument auto . mconcat $ [
      metavar "BUILD_ID"
    , help "Unique build identifier."
    ]

versionP :: Options.Parser ()
versionP =
  Options.flag' () . mconcat $ [
      Options.short 'V'
    , Options.long "version"
    , Options.help "Version information"
    ]

orDie :: (e -> Text) -> EitherT e IO a -> IO a
orDie render e =
  runEitherT e >>=
    either (\err -> (hPutStrLn stderr . T.unpack . render) err >> exitFailure) pure

command' :: String -> String -> Options.Parser a -> Options.Mod Options.CommandFields a
command' label description parser =
  Options.command label (Options.info parser (Options.progDesc description))

dispatch :: Options.Parser a -> IO a
dispatch p = do
  Options.customExecParser
    (Options.prefs . mconcat $ [
        Options.showHelpOnEmpty
      , Options.showHelpOnError
      ])
    (Options.info
      (p <**> Options.helper)
      (mconcat [
          Options.fullDesc
        , Options.progDesc "Manage and interact with boris builds."
        , Options.header "boris build bot"
        ]))

textRead :: Options.ReadM Text
textRead =
  Text.pack <$> Options.str
