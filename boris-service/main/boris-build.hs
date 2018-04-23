{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_boris_service
import           DependencyInfo_ambiata_boris_service

import           Boris.Core.Data
import qualified Boris.Service.Boot as Boot
import qualified Boris.Service.Discover as Discover

import           Data.Default (def)
import           Data.String (String)

import qualified Nest

import           Network.Connection (ProxySettings (..))
import           Network.HTTP.Client (ManagerSettings, newManager)
import           Network.HTTP.Client.TLS (mkManagerSettings)

import           Options.Applicative

import           P

import           Snooze.Balance.Data (BalanceTable (..), BalanceEntry (..), Host (..), Port (..), balanceTableStatic)
import           Snooze.Balance.Control (BalanceConfig (..))

import           System.Exit (exitSuccess)
import           System.Environment (lookupEnv)
import           System.IO

import           X.Options.Applicative
import           X.Control.Monad.Trans.Either.Exit (orDie)

data Cli =
    BuildCommand BuildId Project Repository Build (Maybe Ref)
  | DiscoverCommand BuildId Project Repository
    deriving (Eq, Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch parser >>= \sc ->
    case sc of
      VersionCommand ->
        putStrLn buildInfoVersion >> exitSuccess
      DependencyCommand ->
        mapM putStrLn dependencyInfo >> exitSuccess
      RunCommand DryRun c ->
        print c >> exitSuccess
      RunCommand RealRun c ->
        run c

parser :: Parser (SafeCommand Cli)
parser =
  safeCommand . subparser . mconcat $ [
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
  DiscoverCommand buildid project repository -> do
    let
      path =
        WorkspacePath "."

    Boot.Boot logs discovers _ <-
      Nest.force $ Boot.boot mkBalanceConfig

    orDie Discover.renderDiscoverError $
      Discover.discover logs discovers path buildid project repository

    exitSuccess

  BuildCommand _buildid _project _repository _build _ref -> do
    Boot.Boot _logs _ _build <-
      Nest.force $ Boot.boot mkBalanceConfig

    -- TODO implement
    exitSuccess

projectP :: Parser Project
projectP =
  fmap Project . argument textRead . mconcat $ [
      metavar "PROJECT"
    , help "Project name, this relates to the project name configured in boris, e.g. boris."
    ]

repositoryP :: Parser Repository
repositoryP =
  fmap Repository . argument textRead . mconcat $ [
      metavar "REPOSITORY"
    , help "Repository url."
    ]

buildP :: Parser Build
buildP =
  fmap Build . argument textRead . mconcat $ [
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
  fmap BuildId . argument textRead . mconcat $ [
      metavar "BUILD_ID"
    , help "Unique build identifier."
    ]

mkBalanceConfig :: IO BalanceConfig
mkBalanceConfig = do
  ms <- getManagerSettings
  mgr <- newManager ms
  h <- Nest.force $ Host <$> Nest.string "HOST"
  p <- Nest.force $ Port <$> Nest.numeric "PORT" `Nest.withDefault` 9999
  t <- balanceTableStatic $ BalanceTable [BalanceEntry h p]
  pure $ BalanceConfig t mempty mgr

socksProxyKey :: String
socksProxyKey =
  "SOCKS_PROXY"

getManagerSettings :: IO ManagerSettings
getManagerSettings = do
  msocks <- lookupEnv socksProxyKey
  pure . mkManagerSettings def $
    SockSettingsEnvironment (Just socksProxyKey) <$ msocks
