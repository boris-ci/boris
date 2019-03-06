{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Boris.Core.Data.Workspace
import           Boris.Core.Data.Workspace
import qualified Boris.Client.Config as Config
import qualified Boris.Service.Boot as Boot
import           Boris.Service.Daemon
import           Boris.Prelude
import           Boris.Git.Pin (newPin)

import           Control.Concurrent.Async (async, waitCatch)

import           Data.Default (def)
import qualified Data.List as L
import           Data.String (String)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Nest

import           Network.Connection (ProxySettings (..))
import           Network.HTTP.Client (ManagerSettings, newManager)
import           Network.HTTP.Client.TLS (mkManagerSettings)

import           System.Environment (lookupEnv)
import           System.Exit (exitSuccess, exitFailure)
import           System.IO


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  pin <- newPin
--  environment <- Environment <$> text "BORIS_ENVIRONMENT"
--  T.putStrLn $ "boris-environment: " <> (T.pack . show) environment
  work <- WorkspacePath <$> text "BORIS_WORKSPACE_PATH"
  n <- intOr "BORIS_WORK_THREADS" 1

  let
    configure =
      orDie Config.renderBorisConfigureError . newEitherT $
        Config.configure

  Boot.Boot logx discoverx buildx <-
    Nest.force $ Boot.boot configure

  asyncs <- mapM async $ L.replicate n (run logx buildx discoverx work pin)
  results <- forM asyncs $ waitCatch
  forM_ results $ \result -> case result of
    Left e ->
      T.hPutStrLn stderr . mconcat $ ["An unexpected error occurred waiting for work thread to finish: ", T.pack . show $ e]
    Right _ ->
      pure ()
  forM_ results $ \result -> case result of
    Left _ ->
      exitFailure
    Right _ ->
      pure ()
  exitSuccess

intOr :: String -> Int -> IO Int
intOr e dfault =
  lookupEnv e >>=
    maybe
      (bomb . T.pack $ e <> " is a required environment variable to start boris.")
      (fmap Just . fromMaybeM (bomb . T.pack $ e <> " is not a valid int and is a required environment variable to start boris.") . readMaybe) >>= fromMaybeM (pure dfault)

text :: String -> IO Text
text e =
  lookupEnv e >>=
    maybe (bomb . T.pack $ e <> " is a required environment variable to start boris.") (pure . T.pack)

bomb :: Text -> IO a
bomb msg =
  T.hPutStrLn stderr msg >> exitFailure

orDie :: (e -> Text) -> EitherT e IO a -> IO a
orDie render e =
  runEitherT e >>=
    either (\err -> (hPutStrLn stderr . T.unpack . render) err >> exitFailure) pure
