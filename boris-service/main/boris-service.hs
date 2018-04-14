{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Boris.Core.Data
import           Boris.Queue (BuildQueue (..))
import qualified Boris.Service.Boot as Boot
import           Boris.Service.Daemon

import           Control.Concurrent.Async (async, waitCatch)

import           Data.Default (def)
import qualified Data.List as L
import           Data.String (String)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Mismi (renderRegionError, discoverAWSEnv)
import           Mismi.DynamoDB.Control (configureRetries)

import qualified Nest

import           Network.Connection (ProxySettings (..))
import           Network.HTTP.Client (ManagerSettings, newManager)
import           Network.HTTP.Client.TLS (mkManagerSettings)

import           P

import           Snooze.Balance.Data (BalanceTable (..), BalanceEntry (..), Host (..), Port (..), balanceTableStatic)
import           Snooze.Balance.Control (BalanceConfig (..))

import           System.Environment (lookupEnv)
import           System.Exit (exitSuccess, exitFailure)
import           System.IO

import           Twine.Data.Pin (newPin)


import           X.Control.Monad.Trans.Either.Exit (orDie)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  pin <- newPin
  env <- orDie renderRegionError discoverAWSEnv
  environment <- Environment <$> text "BORIS_ENVIRONMENT"
  T.putStrLn $ "boris-environment: " <> (T.pack . show) environment
  queue <- BuildQueue <$> text "BORIS_BUILD_QUEUE"
  work <- WorkspacePath <$> text "BORIS_WORKSPACE_PATH"
  n <- intOr "BORIS_WORK_THREADS" 1
  let
    cenv = configureRetries env

  Boot.Boot logx discoverx buildx <-
    Nest.force $ Boot.boot mkBalanceConfig

  asyncs <- mapM async $ L.replicate n (run logx buildx discoverx cenv queue work pin)
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
