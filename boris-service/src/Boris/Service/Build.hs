{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Build (
    BuilderError (..)
  , builder
  , renderBuilderError
  ) where

import           Boris.Build
import           Boris.Core.Data
import           Boris.Service.Git
import           Boris.Service.Log
import           Boris.Service.Workspace
import           Boris.Store.Build (BuildCancelled (..))
import qualified Boris.Store.Build as SB
import           Boris.Queue (RequestBuild (..))
import           Boris.X (WithEnv (..))
import qualified Boris.X as X

import           Control.Concurrent.Async (async, waitEitherCancel)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)

import qualified Data.Text.IO as T

import           Jebediah.Data (GroupName (..), StreamName (..))

import           Mismi (Error, runAWS, runAWST, renderError)
import           Mismi.Amazonka (Env)

import           P

import           System.IO (IO)

import           Twine.Snooze (snooze, seconds)

import           X.Control.Monad.Trans.Either (EitherT, newEitherT, runEitherT)

data BuilderError =
    BuildAwsError Error

-- |
-- Basic executor flow:
--
--  * pick a build off the queue.
--
--  * we try to acknowledge it, dropping it if someone else has already
--    picked up the build.
--
--  * we create a new workspace based on the build-id.
--
--  * we initialise the workspace:
--     * get the repository
--     * read the configs
--     * determine the specifics of this build
--     * see 'Boris.Service.Git' for all the details.
--
--  * we run the actual build:
--     * See 'Boris.Build' for all the details.
--
--  * we record the results.
--
--
builder :: Env -> Environment -> WorkspacePath -> RequestBuild -> EitherT BuilderError IO ()
builder env e w request = do
  let
    project = requestBuildProject request
    build = requestBuildName request
    buildId = requestBuildId request
    repository = requestBuildRepository request
    mref = requestBuildRef request
    gname = GroupName $ mconcat ["boris.", renderEnvironment e]
    sname = StreamName . renderBuildId $ buildId

  out <- runAWST env BuildAwsError . lift $
    newLogger gname sname

  liftIO . T.putStrLn $ "acknowledge: " <> renderBuildId buildId
  ack <- runAWST env BuildAwsError . lift $
    SB.acknowledge e buildId gname sname

  case ack of
    Accept -> do
      withWorkspace w buildId $ \workspace -> do
        bootstrap <- liftIO . runEitherT $
          initialise out out workspace build repository mref

        case bootstrap of
          Left err -> do
            X.xPutStrLn out $ mconcat ["| boris:ko | ", renderInitialiseError err]

            runAWST env BuildAwsError . lift $ do
              liftIO . T.putStrLn $ "deindex: " <> renderBuildId buildId
              SB.deindex e project build buildId
              liftIO . T.putStrLn $ "complete: " <> renderBuildId buildId
              SB.complete e buildId BuildKo

          Right instant -> do
            let
              ref = buildRef instant
              commit = buildCommit instant
              specification = buildSpecification instant

              -- FIX inherit list should be defined externally
              context = [
                  InheritEnv "AWS_DEFAULT_REGION"
                , InheritEnv "AMBIATA_ARTEFACTS_MASTER"
                , InheritEnv "AMBIATA_HADDOCK"
                , InheritEnv "AMBIATA_DOWNLOAD"
                , InheritEnv "AMBIATA_MAFIA_CACHE"
                , InheritEnv "AMBIATA_ARTEFACTS_BRANCHES"
                , InheritEnv "AMBIATA_BENCHMARK_RESULTS"
                , InheritEnv "AMBIATA_TEST_BUCKET"
                , InheritEnv "AMBIATA_IVY_PAY"
                , InheritEnv "AMBIATA_IVY_OSS"
                , InheritEnv "AMBIATA_DOC"
                , InheritEnv "AMBIATA_DISPENSARY"
                , InheritEnv "HOME"
                , InheritEnv "TMPDIR"
                , InheritEnv "PATH"
                , InheritEnv "LOCALE"
                , InheritEnv "LC_COLLATE"
                , InheritEnv "LANG"
                , InheritEnv "HOSTNAME"
                , InheritEnv "SHELL"
                , InheritEnv "TERM"
                , InheritEnv "USER"
                , InheritEnv "RBENV_ROOT"
                , InheritEnv "HADOOP_USER_NAME"
                , InheritEnv "HADOOP_CONF_BASE"
                , SetEnv "BORIS_BUILD_ID" (renderBuildId buildId)
                , SetEnv "BORIS_PROJECT" (renderProject project)
                , SetEnv "BORIS_BUILD" (renderBuild build)
                , SetEnv "BORIS_REF" (renderRef ref)
                , SetEnv "BORIS_COMMIT" (renderCommit commit)
                ]

            runAWST env BuildAwsError . lift $ do
              liftIO . T.putStrLn $ "index: " <> renderBuildId buildId
              SB.index e project build buildId ref commit


            let
              runner =
                runEitherT $
                  runBuild out out workspace specification context

              heartbeater =
                runEitherT $ do
                  heart <- runAWS env $ do
                    SB.heartbeat e buildId
                  case heart of
                    BuildCancelled ->
                      pure ()
                    BuildNotCancelled -> do
                      liftIO . snooze . seconds $ 30
                      newEitherT heartbeater

            runner' <- liftIO . async $ runner
            heartbeater' <- liftIO . async $ heartbeater
            state <- liftIO $ waitEitherCancel heartbeater' runner'

            result <- case state of
              Right (Left err) -> do
                X.xPutStrLn out $ mconcat ["| boris:ko | ", renderBuildError err]
                pure $ BuildKo
              Right (Right _) -> do
                X.xPutStrLn out $ "| boris:ok |"
                pure $ BuildOk
              Left (Left err) -> do
                X.xPutStrLn out $ mconcat ["| boris:ko:heartbeat-error | ", renderError err]
                pure $ BuildKo
              Left (Right _) -> do
                X.xPutStrLn out $ mconcat ["| boris:ko:cancelled | "]
                pure $ BuildKo

            runAWST env BuildAwsError . lift $ do
              liftIO . T.putStrLn $ "complete: " <> renderBuildId buildId
              SB.complete e buildId result

    AlreadyRunning ->
        pure ()

renderBuilderError :: BuilderError -> Text
renderBuilderError err =
  case err of
    BuildAwsError e ->
      mconcat ["An AWS error occurred trying to obtain a build to run: ", renderError e]
