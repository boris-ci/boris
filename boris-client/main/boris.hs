{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_boris_client
import           DependencyInfo_boris_client

import           Boris.Core.Data.Agent
import           Boris.Core.Data.Build
import           Boris.Core.Data.Log
import           Boris.Core.Data.Project
import           Boris.Client.Error
import qualified Boris.Client.Config as Config
import qualified Boris.Client.Build as Build
import qualified Boris.Client.Project as Project
import qualified Boris.Client.Log as Log
import qualified Boris.Client.Network as Network

import           Boris.Prelude
import           Control.Concurrent (threadDelay)
import           Control.Monad.IO.Class (liftIO)

import           Data.Default.Class (def)
import           Data.String (String)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time (UTCTime, diffUTCTime, formatTime, defaultTimeLocale)

import           Network.Connection (ProxySettings (..))
import           Network.HTTP.Client (ManagerSettings, newManager)
import           Network.HTTP.Client.TLS (mkManagerSettings)

import qualified Options.Applicative as Options


import qualified System.Exit as Exit
import qualified System.Environment as Environment
import           System.IO (IO, FilePath)
import qualified System.IO as IO


data Cli =
    Trigger Project Build (Maybe Ref)
  | Discover Project
  | Cancel BuildId
  | List (Maybe Project) (Maybe Build)
  | Status BuildId
  | Log BuildId
  | Ignore Project Build
  | Rebuild BuildId
  | Queue
  | Version
    deriving (Eq, Show)

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  dispatch (Version <$ versionP <|> parser) >>= run

parser :: Options.Parser Cli
parser =
  Options.hsubparser . mconcat $ [
      command' "build" "Trigger a build"  $
        Trigger
          <$> projectP
          <*> buildP
          <*> optional refP
    , command' "discover" "Probe for builds to trigger for a project" $
        Discover
          <$> projectP
    , command' "cancel" "Cancel a build" $
        Cancel
          <$> buildIdP
    , command' "list" "list of projects / builds" $
        List
          <$> optional projectP
          <*> optional buildP
    , command' "status" "status of build" $
        Status
          <$> buildIdP
    , command' "log" "Log of a build" $
        Log
          <$> buildIdP
    , command' "ignore" "Ignore a build" $
        Ignore
          <$> projectP
          <*> buildP
    , command' "rebuild" "Rebuild a build" $
        Rebuild
          <$>  buildIdP
    , command' "queue" "Get the current queue number" $
        pure Queue
    ]

run :: Cli -> IO ()
run c = case c of
  Version ->
    IO.putStrLn buildInfoVersion >> Exit.exitSuccess
  Trigger p b ref -> do
    boris <- runOrFlailT Config.renderBorisConfigureError $ Config.configureT
    r <- runOrFlailT renderBorisError . Network.runRequestT boris $ Build.trigger p b ref
    IO.print r
    {--

    d <- orDie renderBorisHttpClientError $ B.trigger bc p b ref
    Text.hPutStrLn stderr $ mconcat ["boris submitted [", renderBuildId . buildDataId $ d, "]"]
    let
      i = buildDataId d

      waitForLog = do
        liftIO . Text.putStrLn $ "Waiting for build to start..."
        liftIO $ threadDelay 1000000
        r <- B.fetch bc i
        case fmap buildDataId r of
          Nothing ->
            waitForLog
          Just i' ->
            L.fetch bc i'

      taillog (DBLog ls) =
        Text.putStrLn $ renderDBLogs ls

    l <- orDie renderBorisHttpClientError waitForLog
    taillog l

    exitSuccess

--}

  Discover p -> do
    error "todo"
    {--
    void . orDie renderBorisHttpClientError $ P.discover bc p
    Text.putStrLn . mconcat $ ["Discovery kicked off for project ", renderProject p]
    exitSuccess
    --}

  Cancel i -> do
    error "todo"
    {--
    void . orDie renderBorisHttpClientError $ B.cancel bc i
    Text.putStrLn . mconcat $ ["Cancelled build #", renderBuildId i]
    exitSuccess
--}
  List pp bb -> do
    error "todo"
    {--
    bc <- mkBalanceConfig
    case (pp, bb) of
      (Nothing, Nothing) ->
        orDie renderBorisHttpClientError $
          P.list bc >>= mapM_ (liftIO . Text.putStrLn . renderProject)
      (Just p, Nothing) ->
        orDie renderBorisHttpClientError $
          P.fetch bc p >>= mapM_ (liftIO . Text.putStrLn . renderBuild)
      (Just p, Just b) ->
        orDie renderBorisHttpClientError $ do
          tree <- B.list bc p b
          for_ tree $ \t ->
            for_ (buildTreeRefs t) $ \(BuildTreeRef r is) -> liftIO $ do
              Text.putStrLn . renderRef $ r
              forM_ is $ \i -> do
                Text.putStr "\t"
                Text.putStrLn . renderBuildId $ i
      (Nothing, Just _) ->
        bomb "Can not specify build without project."
--}
  Status i -> do
    error "Todo"
    {--
    rr <- orDie renderBorisHttpClientError $ B.fetch bc i
    case rr of
      Nothing -> do
        Text.putStrLn . mconcat $ ["No build [", renderBuildId i, "] found."]
        exitFailure
      Just r -> do
        Text.putStrLn  $ renderBuildData r i
        exitSuccess
        --}
  Log i -> do
    error "todo"
  {--
    ll <- orDie renderBorisHttpClientError $ L.fetch bc i
    case ll of
      DBLog ls -> do
        Text.putStrLn $ renderDBLogs ls
        exitSuccess
        --}
  Ignore p b -> do
    error "todo"
    {--
    void . orDie renderBorisHttpClientError $ B.ignore bc p b True
    Text.putStrLn "Build ignored"
    exitSuccess
--}
  Rebuild i -> do
    error "todo"
    {--
    bc <- mkBalanceConfig
    rr <- orDie renderBorisHttpClientError $ B.rebuild bc i
    case rr of
      Nothing -> do
        Text.putStrLn . mconcat $ ["No build [", renderBuildId i, "] found."]
        exitFailure
      Just r -> do
        Text.putStrLn $ renderBuildData r i
        exitSuccess
--}
  Queue -> do
    error "todo"
{--
    rr <- orDie renderBorisHttpClientError $ B.queue bc
    case rr of
      Nothing -> do
        Text.putStrLn "Unable to retrieve queue information"
        exitFailure
      Just q -> do
        Text.putStrLn . Text.pack . show . getQueueSize $ q
        exitSuccess
--}

renderBuildData :: BuildData -> BuildId -> Text
renderBuildData r _i =
   Text.unlines $ [
       mconcat ["id: ", renderBuildId . buildDataId $ r]
     , mconcat ["project: ", renderProject . buildDataProject $ r]
     , mconcat ["build: ", renderBuild . buildDataBuild $ r]
     , mconcat ["ref: ", maybe "n/a" renderRef . buildDataRef $ r]
     , mconcat ["queued-at: ", maybe "n/a" renderTime . buildDataQueueTime $ r]
     , mconcat ["started-at: ", maybe "n/a" renderTime . buildDataStartTime $ r]
     , mconcat ["end-at: ", maybe "n/a" renderTime . buildDataEndTime $ r]
     , mconcat ["heartbeat-at: ", maybe "n/a" renderTime . buildDataHeartbeatTime $ r]
     , mconcat ["duration: ", maybe "n/a" (uncurry renderDuration) $ (,) <$> buildDataStartTime r <*> buildDataEndTime r]
     , mconcat ["result: ", maybe "n/a" (\br -> case br of BuildOk -> "successful"; BuildKo -> "failure") . buildDataResult $ r]
     ]


projectP :: Options.Parser Project
projectP =
  fmap Project . Options.argument textRead . mconcat $ [
      Options.metavar "PROJECT"
    , Options.help "Project name, this relates to the project name configured in boris, e.g. boris."
    ]

buildP :: Options.Parser Build
buildP =
  fmap Build . Options.argument textRead . mconcat $ [
      Options.metavar "BUILD"
    , Options.help "Build name, this relates to the project name configured in repository, e.g. dist, branches."
    ]

refP :: Options.Parser Ref
refP =
  fmap Ref . Options.argument textRead . mconcat $ [
      Options.metavar "REF"
    , Options.help "A specific git ref to build, e.g. master, topic/hax."
    ]

buildIdP :: Options.Parser BuildId
buildIdP =
  fmap BuildId . Options.argument Options.auto . mconcat $ [
      Options.metavar "BUILD_ID"
    , Options.help "Unique build identifier."
    ]

borisrefP :: Options.Parser FilePath
borisrefP =
 Options.strOption . mconcat $ [
      Options.long "boris-ref"
    , Options.metavar "FILEPATH"
    , Options.help "A boris ref file, e.g. boris-git.toml."
    ]

boriscommandP :: Options.Parser FilePath
boriscommandP =
  Options.strOption . mconcat $ [
      Options.long "boris-command"
    , Options.metavar "FILEPATH"
    , Options.help "A boris command file, e.g. boris.toml."
    ]

versionP :: Options.Parser ()
versionP =
  Options.flag' () . mconcat $ [
      Options.short 'V'
    , Options.long "version"
    , Options.help "Version information"
    ]

text :: String -> IO Text
text e =
  Environment.lookupEnv e >>=
    maybe (bomb . Text.pack $ e <> " is a required environment variable to start boris.") (pure . Text.pack)

intOr :: String -> Int -> IO Int
intOr e dfault =
  Environment.lookupEnv e >>=
    maybe
      (bomb . Text.pack $ e <> " is a required environment variable to start boris.")
      (fmap Just . fromMaybeM (bomb . Text.pack $ e <> " is not a valid int and is a required environment variable to start boris.") . readMaybe) >>= fromMaybeM (pure dfault)

bomb :: Text -> IO a
bomb msg =
  Text.hPutStrLn IO.stderr msg >> Exit.exitFailure

socksProxyKey :: String
socksProxyKey =
  "SOCKS_PROXY"

getManagerSettings :: IO ManagerSettings
getManagerSettings = do
  msocks <- Environment.lookupEnv socksProxyKey
  pure . mkManagerSettings def $
    SockSettingsEnvironment (Just socksProxyKey) <$ msocks

renderTime :: UTCTime -> Text
renderTime =
  Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

renderDuration :: UTCTime -> UTCTime -> Text
renderDuration s e =
  mconcat [Text.pack . show $ ((round (diffUTCTime e s)) :: Integer), "s"]

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

runOrFlailT :: (e -> Text) -> EitherT e IO a -> IO a
runOrFlailT handler =
  runOrFlail handler . runEitherT

runOrFlail :: (e -> Text) -> IO (Either e a) -> IO a
runOrFlail handler action =
  action >>= either (flail . handler) pure

flail :: Text -> IO a
flail msg = do
  Text.hPutStrLn IO.stderr msg
  Exit.exitFailure

renderBorisError :: BorisError -> Text
renderBorisError e =
  case e of
    -- FUTURE: debug mode that prints message.
    -- FUTURE: Handle specific error codes for better error messages.
    BorisApplicationError code _message ->
      mconcat ["There was an error performing your request [", getErrorCode code, "]."]
    -- FUTURE: debug mode that prints message.
    BorisAuthorizationError code _message ->
      mconcat ["You are not authorized to perform this request [", getErrorCode code, "]."]
--    BorisAuthenticationError _err ->
--      mconcat ["Boris could not authenticate you, please check your credentials and connectivity to Boris. DEBUG: ", Text.pack . show $ _err]
    -- FUTURE: debug mode that prints body + message
    BorisResponseParseError code _body _message ->
      mconcat ["Boris response parse error [", Text.pack . show $ code, "]. Please check connectivity to Boris and retry request."]
    -- FUTURE: debug mode that prints body.
    BorisStatusCodeError code _body ->
      mconcat ["Boris status code error [", Text.pack . show $ code, "]. Please check connectivity to Boris and retry request."]
    BorisUrlParseError message ->
      mconcat ["Boris client url-parse error [", message, "]. Check you are running the latest client version, and raise a supportissue if this issue persists ."]
