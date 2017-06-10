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
import qualified Boris.Store.Results as SR
import           Boris.Queue (RequestBuild (..))

import           Control.Concurrent.Async (async, waitEitherCancel)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)

import qualified Data.Text.IO as T

import           Jebediah.Data (LogGroup (..), LogStream (..))

import           Mismi (Error, runAWS, runAWST, renderError)
import           Mismi.Amazonka (Env)

import           P

import           System.IO (IO)

import           Tine.Conduit (WithEnv (..))
import qualified Tine.Conduit as X

import           Twine.Snooze (snooze, seconds)

import           X.Control.Monad.Trans.Either (EitherT, newEitherT, runEitherT)

data BuilderError =
    BuildAwsError Error
  | BuildResultError Text

data LifecycleError =
    LifecycleAwsError Error
  | LifecycleInitialiseError InitialiseError

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
    gname = LogGroup $ mconcat ["boris.", renderEnvironment e]
    sname = LogStream . renderBuildId $ buildId

  runAWST env BuildAwsError . newEitherT . withLogger gname sname $ \out -> runEitherT $ do
    liftIO . T.putStrLn $ "acknowledge: " <> renderBuildId buildId

    initial <- runAWST env BuildAwsError . lift $ do
      SB.heartbeat e buildId

    ack <- runAWST env BuildAwsError . lift $
      SB.acknowledge e buildId gname sname

    case (ack, initial) of
      (Accept, BuildNotCancelled) -> do
        let
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

        heartbeater' <- liftIO . async $ heartbeater
        lifecycle' <- liftIO . async . runEitherT $ lifecycle out env e w project build repository mref buildId

        state <- liftIO $ waitEitherCancel heartbeater' lifecycle'

        result <- case state of
          -- we couldn't send a heartbeat, so had to give up, this should be rare, but can happen.
          Left (Left err) -> do
            X.xPutStrLn out $ mconcat ["| boris:ko:heartbeat-error | ", renderError err]
            pure $ BuildKo

          -- we were cancelled, nothing more to see here.
          Left (Right _) -> do
            X.xPutStrLn out $ mconcat ["| boris:ko:cancelled | "]
            pure $ BuildKo

          -- initialisation error, wasn't even a valid build we ditch it all together (i.e. deindex).
          Right (Left err) -> do
            liftIO . T.putStrLn $ "deindex: " <> renderBuildId buildId
            runAWST env BuildAwsError . lift $ do
              SB.deindex e project build buildId
            X.xPutStrLn out $ mconcat ["| boris:ko:initialization | ", renderLifecycleError err]
            pure $ BuildKo

          -- build completed but failed.
          Right (Right (Left err)) -> do
            X.xPutStrLn out $ mconcat ["| boris:ko | ", renderBuildError err]
            pure $ BuildKo

          -- build completed, defied all odds, and actually succeeded.
          Right (Right (Right ())) -> do
            X.xPutStrLn out $ "| boris:ok |"
            pure $ BuildOk

        runAWST env BuildAwsError $ do
          liftIO . T.putStrLn $ "complete: " <> renderBuildId buildId
          lift $ SB.complete e buildId result
          firstT BuildResultError $
            SR.add e (SR.Result buildId project build (Ref "TODO" {- mref -}) result)

      (AlreadyRunning, BuildCancelled) ->
        pure ()

      (AlreadyRunning, BuildNotCancelled) ->
        pure ()

      (Accept, BuildCancelled) ->
        runAWST env BuildAwsError $ do
          lift $ SB.complete e buildId BuildKo
          firstT BuildResultError $
            SR.add e (SR.Result buildId project build (Ref "TODO" {- mref -}) BuildKo)


lifecycle :: X.Out -> Env -> Environment -> WorkspacePath -> Project -> Build -> Repository -> Maybe Ref -> BuildId -> EitherT LifecycleError IO (Either BuildError ())
lifecycle out env e w project build repository mref buildId =
  withWorkspace w buildId $ \workspace -> do
    instant <- firstT LifecycleInitialiseError $
      initialise out out workspace build repository mref

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

    runAWST env LifecycleAwsError . lift $ do
      liftIO . T.putStrLn $ "index: " <> renderBuildId buildId
      SB.index e project build buildId ref commit

    liftIO . runEitherT $
      runBuild out out workspace specification context

renderBuilderError :: BuilderError -> Text
renderBuilderError err =
  case err of
    BuildAwsError e ->
      mconcat ["An AWS error occurred trying to obtain a build to run: ", renderError e]
    BuildResultError e ->
      mconcat ["Error decoding results: ", e]

renderLifecycleError :: LifecycleError -> Text
renderLifecycleError err =
  case err of
    LifecycleAwsError e ->
      mconcat ["An AWS error occurred trying to update boris store: ", renderError e]
    LifecycleInitialiseError e ->
      mconcat ["An error occurred trying to initialise git repository: ", renderInitialiseError e]
