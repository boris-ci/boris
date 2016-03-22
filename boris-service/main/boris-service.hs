{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Boris.Core.Data
import           Boris.Queue (BuildQueue (..))
import           Boris.Service.Daemon

import           Control.Concurrent.Async (async, waitCatch)

import qualified Data.List as L
import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Mismi (renderRegionError, discoverAWSEnv)

import           P

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
  asyncs <- mapM async $ L.replicate n (run env environment queue work pin)
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
