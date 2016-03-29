{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Listener (
    ListenerError (..)
  , listen
  , renderListenerError
  ) where

import           Boris.Build
import           Boris.Core.Data
import           Boris.Service.Git
import           Boris.Service.Log
import           Boris.Service.Workspace
import qualified Boris.Store.Build as SB
import           Boris.Queue (BuildQueue (..), Request (..))
import qualified Boris.Queue as Q
import           Boris.X (WithEnv (..))
import qualified Boris.X as X

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)

import           Data.Text (Text)
import qualified Data.Text.IO as T

import           Jebediah.Data (GroupName (..), StreamName (..))

import           Mismi (Error, runAWST, renderError)
import           Mismi.Amazonka (Env)

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, bimapEitherT, runEitherT)

data ListenerError =
    ListenerQueueError Q.QueueError
  | ListenerAwsError Error

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
-- Future improvements:
--
--  * We should really be sending a heart-beat to the store so that
--    others can tell if we die.
--
--  * We should also allow for cancelling of a running build.
--
listen :: Env -> Environment -> BuildQueue -> WorkspacePath -> EitherT ListenerError IO ()
listen env e q w = do
  r <- runAWST env ListenerAwsError . bimapEitherT ListenerQueueError id $
    Q.get q

  forM_ r $ \request -> do
    let
      project = requestProject request
      build = requestBuild request
      buildId = requestBuildId request
      repository = requestRepository request
      mref = requestRef request
      gname = GroupName $ mconcat ["boris.", renderEnvironment e]
      sname = StreamName . renderBuildId $ buildId

    out <- runAWST env ListenerAwsError . lift $
      newLogger gname sname

    liftIO . T.putStrLn $ "acknowledge: " <> renderBuildId buildId
    ack <- runAWST env ListenerAwsError . lift $
      SB.acknowledge e buildId gname sname

    case ack of
      Accept -> do
        withWorkspace w buildId $ \workspace -> do
          bootstrap <- liftIO . runEitherT $
            initialise out out workspace build repository mref

          case bootstrap of
            Left err -> do
              X.xPutStrLn out $ mconcat ["| boris:ko | ", renderInitialiseError err]

              runAWST env ListenerAwsError . lift $ do
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
                  , SetEnv "BORIS_BUILD_ID" (renderBuildId buildId)
                  , SetEnv "BORIS_PROJECT" (renderProject project)
                  , SetEnv "BORIS_BUILD" (renderBuild build)
                  , SetEnv "BORIS_REF" (renderRef ref)
                  , SetEnv "BORIS_COMMIT" (renderCommit commit)
                  ]

              runAWST env ListenerAwsError . lift $ do
                liftIO . T.putStrLn $ "index: " <> renderBuildId buildId
                SB.index e project build buildId ref commit

              result <- liftIO . runEitherT $
                runBuild out out workspace specification context

              case result of
                Left err ->
                  X.xPutStrLn out $ mconcat ["| boris:ko | ", renderBuildError err]
                Right _ ->
                  X.xPutStrLn out $ "| boris:ok |"

              runAWST env ListenerAwsError . lift $ do
                liftIO . T.putStrLn $ "complete: " <> renderBuildId buildId
                SB.complete e buildId (bool BuildKo BuildOk $ isRight result)

      AlreadyRunning ->
        pure ()

renderListenerError :: ListenerError -> Text
renderListenerError err =
  case err of
    ListenerQueueError e ->
      mconcat ["An error occurred trying to obtain a build to run: ", Q.renderQueueError e]
    ListenerAwsError e ->
      mconcat ["An AWS error occurred trying to obtain a build to run: ", renderError e]
