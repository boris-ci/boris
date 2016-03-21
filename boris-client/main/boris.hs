{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_boris_client

import           Boris.Core.Data
import           Boris.Store.Build (BuildData (..), LogData (..))
import           Boris.Client.Http (renderBorisHttpClientError)
import qualified Boris.Client.Build as B
import qualified Boris.Client.Project as P
import qualified Boris.Client.Log as L

import           Control.Concurrent (threadDelay)
import           Control.Monad.IO.Class (liftIO)

import           Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Mismi (renderRegionError, discoverAWSEnv, runAWS, renderError)

import           Network.HTTP.Client (newManager, defaultManagerSettings)

import           Options.Applicative

import           P

import           Snooze.Balance.Data (BalanceTable (..), BalanceEntry (..), Host (..), Port (..), balanceTableStatic)
import           Snooze.Balance.Control (BalanceConfig (..))

import           System.IO
import           System.Exit (exitSuccess, exitFailure)
import           System.Environment (lookupEnv)

import           X.Options.Applicative
import           X.Control.Monad.Trans.Either.Exit (orDie)

data Tail =
    Tail
  | NoTail
    deriving (Eq, Show)

data Cli =
    Trigger Tail Project Build (Maybe Ref)
  | Status (Maybe Project) (Maybe Build)
  | Log BuildId
    deriving (Eq, Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  environment <- Environment <$> text "BORIS_ENVIRONMENT"
  dispatch parser >>= \sc ->
    case sc of
      VersionCommand ->
        putStrLn buildInfoVersion >> exitSuccess
      RunCommand DryRun c ->
        print c >> exitSuccess
      RunCommand RealRun c ->
        run environment c

parser :: Parser (SafeCommand Cli)
parser =
  safeCommand . subparser . mconcat $ [
      command' "build" "Trigger a build" $
        Trigger
          <$> tailP
          <*> projectP
          <*> buildP
          <*> optional refP
    , command' "status" "Status of a project / build" $
        Status
          <$> optional projectP
          <*> optional buildP
    , command' "log" "Log of a build" $
        Log
          <$> buildIdP
    ]

run :: Environment -> Cli -> IO ()
run e c = case c of
  Trigger t p b _r -> do
    bc <- mkBalanceConfig
    d <- orDie renderBorisHttpClientError $ B.trigger bc p b
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
      l <- orDie renderBorisHttpClientError waitForLog
      env <- orDie renderRegionError discoverAWSEnv
      orDie renderError . runAWS env $ L.source' (logGroup l) (logStream l) $$
        CL.mapM_ (liftIO . T.putStrLn)
  -- FIX rename to list, make status [build-id] for determining success etc...
  Status pp bb -> do
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
  Log i -> do
    env <- orDie renderRegionError discoverAWSEnv
    orDie renderError . runAWS env $ L.source e i $$ CL.mapM_ (liftIO . T.putStrLn)

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
  mgr <- newManager defaultManagerSettings
  h <- Host <$> text "HOST"
  p <- Port <$> intOr "PORT" 11111
  t <- balanceTableStatic . BalanceTable (BalanceEntry h p) $ []
  pure $ BalanceConfig t mempty mgr
