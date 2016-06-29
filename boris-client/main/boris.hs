{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_boris_client
import           DependencyInfo_ambiata_boris_client

import           Boris.Core.Data
import qualified Boris.Core.Serial.Command as S
import qualified Boris.Core.Serial.Ref as S
import           Boris.Store.Build (BuildData (..), LogData (..))
import           Boris.Client.Http (renderBorisHttpClientError)
import qualified Boris.Client.Build as B
import qualified Boris.Client.Project as P
import qualified Boris.Client.Log as L

import           Control.Concurrent.Async (async, waitEitherCancel)
import           Control.Concurrent (threadDelay)
import           Control.Monad.IO.Class (liftIO)

import           Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import           Data.Default (def)
import           Data.String (String)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time (UTCTime, diffUTCTime, formatTime, defaultTimeLocale)

import           Mismi (renderRegionError, discoverAWSEnv)

import           Network.Connection (ProxySettings (..))
import           Network.HTTP.Client (ManagerSettings, newManager)
import           Network.HTTP.Client.TLS (mkManagerSettings)

import           Options.Applicative

import           P

import           Snooze.Balance.Data (BalanceTable (..), BalanceEntry (..), Host (..), Port (..), balanceTableStatic)
import           Snooze.Balance.Control (BalanceConfig (..))

import           System.Exit (exitSuccess, exitFailure)
import           System.Environment (lookupEnv)
import           System.IO

import           X.Options.Applicative
import           X.Control.Monad.Trans.Either (EitherT, newEitherT, runEitherT)
import           X.Control.Monad.Trans.Either.Exit (orDie)

data Tail =
    Tail
  | NoTail
    deriving (Eq, Show)

data Cli =
      RemoteCommand RemoteCommand
    | LocalCommand LocalCommand
    deriving (Eq, Show)

data RemoteCommand =
    Trigger Tail Project Build (Maybe Ref)
  | Discover Project
  | Cancel BuildId
  | List (Maybe Project) (Maybe Build)
  | Status BuildId
  | Log BuildId
  | Ignore Project Build
    deriving (Eq, Show)

data LocalCommand =
  Validate (Maybe FilePath) (Maybe FilePath)
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
        case c of
          RemoteCommand r -> do
            environment <- Environment <$> text "BORIS_ENVIRONMENT"
            run environment r
          LocalCommand r ->
            local r

parser :: Parser (SafeCommand Cli)
parser =
  safeCommand . subparser . mconcat $ [
      command' "build" "Trigger a build" . fmap RemoteCommand $
        Trigger
          <$> tailP
          <*> projectP
          <*> buildP
          <*> optional refP
    , command' "discover" "Probe for builds to trigger for a project" . fmap RemoteCommand $
        Discover
          <$> projectP
    , command' "cancel" "Cancel a build" . fmap RemoteCommand $
        Cancel
          <$> buildIdP
    , command' "list" "list of projects / builds" . fmap RemoteCommand $
        List
          <$> optional projectP
          <*> optional buildP
    , command' "status" "status of build" . fmap RemoteCommand $
        Status
          <$> buildIdP
    , command' "log" "Log of a build" . fmap RemoteCommand $
        Log
          <$> buildIdP
    , command' "ignore" "Ignore a build" . fmap RemoteCommand $
        Ignore
          <$> projectP
          <*> buildP
    , command' "validate" "Validate a configuration" . fmap LocalCommand $
        Validate
          <$> optional borisrefP
          <*> optional boriscommandP
    ]

run :: Environment -> RemoteCommand -> IO ()
run e c = case c of
  Trigger t p b ref -> do
    bc <- mkBalanceConfig
    d <- orDie renderBorisHttpClientError $ B.trigger bc p b ref
    T.hPutStrLn stderr $ mconcat ["boris submitted [", renderBuildId . buildDataId $ d, "]"]
    when (t == Tail) $ do
      let
        i = buildDataId d

        waitForLog = do
          liftIO . T.putStrLn $ "Waiting for build to start..."
          liftIO $ threadDelay 1000000
          r <- B.fetch bc i
          case fmap buildDataLog r of
            Nothing ->
              waitForLog
            Just Nothing ->
              waitForLog
            Just (Just l) ->
              pure l

        waitForStatus = runEitherT $ do
          liftIO $ threadDelay 1000000
          r <- B.fetch bc i
          case fmap buildDataResult r of
            Nothing ->
              newEitherT waitForStatus
            Just Nothing ->
              newEitherT waitForStatus
            Just (Just x) -> do
              liftIO $ threadDelay 5000000
              pure x

        taillog env l =
          L.source' env (logGroup l) (logStream l) $$
            CL.mapM_ (liftIO . T.putStrLn)

      l <- orDie renderBorisHttpClientError waitForLog
      env <- orDie renderRegionError discoverAWSEnv
      as <- async waitForStatus
      at <- async $ taillog env l
      f <- waitEitherCancel as at

      case f of
        Left (Left err) ->
          bomb $ renderBorisHttpClientError err
        Left (Right BuildOk) ->
          exitSuccess
        Left (Right BuildKo) ->
          exitFailure
        Right () ->
          exitSuccess

  Discover p -> do
    bc <- mkBalanceConfig
    void . orDie renderBorisHttpClientError $ P.discover bc p
    T.putStrLn . mconcat $ ["Discovery kicked off for project ", renderProject p]
    exitSuccess

  Cancel i -> do
    bc <- mkBalanceConfig
    void . orDie renderBorisHttpClientError $ B.cancel bc i
    T.putStrLn . mconcat $ ["Cancelled build #", renderBuildId i]
    exitSuccess

  List pp bb -> do
    bc <- mkBalanceConfig
    case (pp, bb) of
      (Nothing, Nothing) ->
        orDie renderBorisHttpClientError $
          P.list bc >>= mapM_ (liftIO . T.putStrLn . renderProject)
      (Just p, Nothing) ->
        orDie renderBorisHttpClientError $
          P.fetch bc p >>= mapM_ (liftIO . T.putStrLn . renderBuild)
      (Just p, Just b) ->
        orDie renderBorisHttpClientError $
          B.list bc p b >>= mapM_ (\(r, is) -> liftIO $ do
            T.putStrLn . renderRef $ r
            forM_ is $ \i -> do
              T.putStr "\t"
              T.putStrLn . renderBuildId $ i)
      (Nothing, Just _) ->
        bomb "Can not specify build without project."
  Status i -> do
    bc <- mkBalanceConfig
    rr <- orDie renderBorisHttpClientError $ B.fetch bc i
    case rr of
      Nothing -> do
        T.putStrLn . mconcat $ ["No build [", renderBuildId i, "] found."]
        exitFailure
      Just r -> do
        T.putStrLn . T.unlines $ [
            mconcat ["id: ", renderBuildId . buildDataId $ r]
          , mconcat ["project: ", renderProject . buildDataProject $ r]
          , mconcat ["build: ", renderBuild . buildDataBuild $ r]
          , mconcat ["ref: ", maybe "n/a" renderRef . buildDataRef $ r]
          , mconcat ["queued-at: ", maybe "n/a" renderTime . buildDataQueueTime $ r]
          , mconcat ["started-at: ", maybe "n/a" renderTime . buildDataStartTime $ r]
          , mconcat ["end-at: ", maybe "n/a" renderTime . buildDataEndTime $ r]
          , mconcat ["heartbeat-at: ", maybe "n/a" renderTime . buildDataHeartbeatTime $ r]
          , mconcat ["duration: ", maybe "n/a" (uncurry renderDuration) $ (,) <$> buildDataStartTime r <*> buildDataEndTime r]
          , mconcat ["log: ", maybe "n/a" (const $ "boris log " <> renderBuildId i) . buildDataLog $ r]
          , mconcat ["result: ", maybe "n/a" (\br -> case br of BuildOk -> "successful"; BuildKo -> "failure") . buildDataResult $ r]
          ]
        exitSuccess
  Log i -> do
    env <- orDie renderRegionError discoverAWSEnv
    L.source env e i $$ CL.mapM_ (liftIO . T.putStrLn)
  Ignore p b -> do
    bc <- mkBalanceConfig
    void . orDie renderBorisHttpClientError $ B.ignore bc p b True
    T.putStrLn "Build ignored"
    exitSuccess

local :: LocalCommand -> IO ()
local c =
  case c of
    Validate g b -> orDie id $ do
      let
        xx :: (Text -> Either e a) -> Maybe FilePath -> EitherT e IO ()
        xx f y = forM_ y $ \file ->
          newEitherT $ f <$> T.readFile file
      firstT S.renderBorisPatternConfigError $ xx S.parsePatternConfig g
      firstT S.renderBorisConfigError $ xx S.parseConfig b

projectP :: Parser Project
projectP =
  fmap Project . argument textRead . mconcat $ [
      metavar "PROJECT"
    , help "Project name, this relates to the project name configured in boris, e.g. boris, mismi."
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

tailP :: Parser Tail
tailP =
  flag NoTail Tail . mconcat $ [
      short 't'
    , long "tail"
    , help "Tail build log after submitting."
    ]

borisrefP :: Parser FilePath
borisrefP =
 strOption . mconcat $ [
      long "boris-ref"
    , metavar "FILEPATH"
    , help "A boris ref file, e.g. boris-git.toml."
    ]

boriscommandP :: Parser FilePath
boriscommandP =
  strOption . mconcat $ [
      long "boris-command"
    , metavar "FILEPATH"
    , help "A boris command file, e.g. boris-toml."
    ]

text :: String -> IO Text
text e =
  lookupEnv e >>=
    maybe (bomb . T.pack $ e <> " is a required environment variable to start boris.") (pure . T.pack)

intOr :: String -> Int -> IO Int
intOr e dfault =
  lookupEnv e >>=
    maybe
      (bomb . T.pack $ e <> " is a required environment variable to start boris.")
      (fmap Just . fromMaybeM (bomb . T.pack $ e <> " is not a valid int and is a required environment variable to start boris.") . readMaybe) >>= fromMaybeM (pure dfault)

bomb :: Text -> IO a
bomb msg =
  T.hPutStrLn stderr msg >> exitFailure

mkBalanceConfig :: IO BalanceConfig
mkBalanceConfig = do
  ms <- getManagerSettings
  mgr <- newManager ms
  h <- Host <$> text "HOST"
  p <- Port <$> intOr "PORT" 11111
  t <- balanceTableStatic . BalanceTable (BalanceEntry h p) $ []
  pure $ BalanceConfig t mempty mgr

socksProxyKey :: String
socksProxyKey =
  "SOCKS_PROXY"

getManagerSettings :: IO ManagerSettings
getManagerSettings = do
  msocks <- lookupEnv socksProxyKey
  pure . mkManagerSettings def $
    SockSettingsEnvironment (Just socksProxyKey) <$ msocks

renderTime :: UTCTime -> Text
renderTime =
  T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

renderDuration :: UTCTime -> UTCTime -> Text
renderDuration s e =
  mconcat [T.pack . show $ ((round (diffUTCTime e s)) :: Integer), "s"]
